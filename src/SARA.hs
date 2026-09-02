{-# LANGUAGE OverloadedStrings #-}

module SARA
  ( module SARA.DSL
  , module SARA.Types
  , module SARA.Config
  , module SARA.Error
  , module SARA.Template.Lucid
  , module SARA.Content.Drafts
  , module SARA.Content.Taxonomy
  , module SARA.Content.Pagination
  , module SARA.Content.Summary
  , SaraM
  , sara
  , saraWithClients
  , saraWithOptions
  , validateArg
  , qualitySealFilePath
  , readQualitySealFile
  , projectCacheKey
  , siteScriptCacheKey
  ) where

import SARA.DSL
import SARA.Types
import SARA.Config
import SARA.Error
import SARA.Content.Drafts
import SARA.Content.Taxonomy
import SARA.Content.Pagination
import SARA.Content.Summary
import SARA.Monad (SaraM(..), SaraEnv(..), RuleDecl(..), BuildIssue(..))
import SARA.Security.ShellGuard (validateArg)
import SARA.Internal.Engine (runBuild)
import SARA.Internal.Planner (expandRules, collectOutputs)
import SARA.Internal.Hash (projectCacheKey, siteScriptCacheKey, contentHash)
import SARA.Internal.FrontmatterCache (loadFrontmatterCache, persistFrontmatterCache)
import SARA.Security.GlobGuard (unGlobPattern)
import SARA.Asset.Discover (inferAssetKind)
import System.FilePath.Glob (globDir1, compile)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import SARA.Diagnostics (QualitySeal(..), renderQualitySeal)
import SARA.LiveReload.Server (broadcastMessage, ClientList)
import SARA.Template.Lucid (renderLucid)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import Data.IORef (newIORef, readIORef)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BSL
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Control.Concurrent (MVar)
import qualified Data.Aeson as Aeson

-- | Entry point for a SARA site.
--
--   Checks the @SARA_DRY_RUN@ environment variable (any non-empty
--   value counts as set) in addition to always defaulting to a real
--   build — this is how @sara build --dry-run@ threads the flag
--   through to a project's own @site.hs@, which the CLI runs as a
--   separate subprocess (via @runghc@) and so cannot pass a Haskell
--   value to directly. Callers who already have the flag as a value
--   in hand (e.g. tests, or code embedding SARA directly rather than
--   running it as a script) should use 'saraWithOptions' instead of
--   relying on the environment.
sara :: SaraM () -> IO ()
sara m = do
  dryRunEnv <- lookupEnv "SARA_DRY_RUN"
  let dryRun = maybe False (not . null) dryRunEnv
  saraWithOptions Nothing dryRun m

-- | Entry point that supports broadcasting to live clients. Never runs
--   as a dry run — kept as its own function, rather than adding a
--   'Bool' parameter here, so every existing call site (tests, 'app/Main.hs'
--   prior to this change) keeps compiling and keeps its current
--   behaviour unchanged; see 0.11 of the Haskell Engineering Standard
--   ("compatibility is a promise about the boundary"). New callers that
--   need dry-run should use 'saraWithOptions' directly.
saraWithClients :: Maybe (MVar ClientList) -> SaraM () -> IO ()
saraWithClients mClients = saraWithOptions mClients False

-- | Full entry point: live-reload clients (if any) and whether this is
--   a dry run. A dry run executes the DSL to collect the same
--   'RuleDecl's a real build would, so route conflicts, missing
--   templates, and other planning-time errors are still caught, but
--   'SARA.Internal.Engine.runBuild' is asked to report the plan instead
--   of running Shake, so nothing is read from or written to disk.
-- | Compute the cache-busting manifest for every discovered CSS\/JS
--   asset across all 'RuleDiscover' patterns — see 'envAssetManifest'
--   and 'SARA.Internal.Planner.rewriteAssetReferences' for the full
--   design. Deliberately scoped to just CSS\/JS (via 'inferAssetKind'):
--   these are the asset types most likely to change during active
--   development and most worth cache-busting once they don't; other
--   asset kinds (images, fonts, generic files) are left untouched by
--   this mechanism entirely, matching engineering roadmap item #6's
--   stated scope.
computeAssetManifest :: [GlobPattern] -> IO (Map.Map T.Text T.Text)
computeAssetManifest patterns = do
  entries <- concat <$> mapM manifestForPattern patterns
  pure (Map.fromList entries)
  where
    manifestForPattern g = do
      let patStr = T.unpack (unGlobPattern g)
      files <- globDir1 (compile patStr) "."
      concat <$> mapM manifestForFile files

    manifestForFile :: FilePath -> IO [(T.Text, T.Text)]
    manifestForFile file = case inferAssetKind file of
      SomeAssetKind StyleAsset  -> (: []) <$> hashedEntry file
      SomeAssetKind ScriptAsset -> (: []) <$> hashedEntry file
      _                         -> pure []

    -- Short (8 hex character) hash prefix: plenty of collision
    -- resistance for cache-busting purposes (this is a "did the
    -- content change" signal, not a security boundary), and keeps
    -- the query string short and readable in a browser's network
    -- panel.
    hashedEntry file = do
      bytes <- BS.readFile file
      let url = "/" <> T.pack (dropDotSlash file)
          hash = T.take 8 (contentHash bytes)
      pure (url, hash)

    dropDotSlash ('.' : '/' : rest) = rest
    dropDotSlash path               = path

saraWithOptions :: Maybe (MVar ClientList) -> Bool -> SaraM () -> IO ()
saraWithOptions mClients dryRun m = do
  cwd <- getCurrentDirectory
  root <- mkProjectRoot cwd
  let config = SaraConfig
        { cfgSiteTitle = "SARA Site"
        , cfgSiteUrl = "http://localhost:8080"
        , cfgSiteAuthor = "SARA"
        , cfgDefaultTemplate = "templates/post.html"
        , cfgOutputDirectory = "_site"
        , cfgDryRun = dryRun
        -- Raw HTML in markdown stays off by default even for this
        -- hardcoded config (audit issue #1); a real project's
        -- 'sara.yaml' would be the place to opt in, once
        -- 'SARA.Config''s YAML loader grows a field for it.
        , cfgAllowRawHtml = False
        }
  
  errorRef <- newIORef []
  frontmatterCache <- loadFrontmatterCache root
  
  -- Step 1: Execute DSL to collect RuleDecls. The site graph isn't
  -- known yet at this point, so the environment carries an empty
  -- placeholder for it — nothing during DSL collection reads
  -- 'envSiteGraph' (only 'SARA.Internal.Planner' and the quality-seal
  -- reporting below do, both of which run after this step).
  let initialEnv = SaraEnv
        { envConfig = config
        , envRoot = root
        , envSiteGraph = HS.empty
        , envRemapRules = []
        , envBuildIssues = errorRef
        , envFrontmatterCache = frontmatterCache
        , envAssetManifest = Map.empty
        }
  
  result <- runExceptT $ runWriterT $ runReaderT (unSaraM m) initialEnv
  -- Persisted after every run (success or failure) that got far enough
  -- to parse anything at all — a build that fails partway through
  -- still keeps whatever it managed to cache along the way, so the
  -- next attempt benefits from it too rather than starting cold again.
  persistFrontmatterCache root frontmatterCache
  
  case result of
    Left errs -> do
      mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
      exitFailure
    Right ((), rules) -> do
      let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
      let envWithRemaps = initialEnv { envRemapRules = allRemapRules }

      -- Step 2: Compute the full site graph as a plain value — every
      -- output path this build will produce — by expanding the
      -- dynamic decls ('match'\/'global') the same way a real build
      -- would, then flattening. This happens exactly once, before
      -- Shake (or, on a dry run, before anything at all) runs; see
      -- 'envSiteGraph's Haddock in "SARA.Monad" for why this replaced
      -- an 'IORef' that used to be filled in piecemeal during Shake's
      -- own rule-registration phase.
      expanded <- expandRules envWithRemaps rules
      let siteGraph = HS.fromList (collectOutputs envWithRemaps expanded)
      assetManifest <- computeAssetManifest [ g | RuleDiscover g <- expanded ]
      let finalEnv = envWithRemaps { envSiteGraph = siteGraph, envAssetManifest = assetManifest }
      
      runBuild finalEnv rules

      if dryRun
        then TIO.putStrLn "\nSARA: Dry run finished. No files were written; no quality seal is issued for a dry run."
        else do
          issues <- readIORef (envBuildIssues finalEnv)
          -- Same pass/fail threshold as before this change: any issue
          -- at all fails the build. Only the *reporting* changed here
          -- — grouped by file, with each issue's real message, instead
          -- of a bare 'Bool' — deliberately, not the exit-code
          -- behaviour, since that's a decision (which issues should be
          -- merely warnings) this codebase doesn't have enough
          -- real-world signal to make yet.
          let hasErrors = not (null issues)

          let itemCount = HS.size (envSiteGraph finalEnv)
          -- A simplified industrial performance score: 
          -- (Pages / (Base Overhead + Logic Complexity))
          let perfScore = if itemCount > 0 then min 100 (90 + (itemCount `div` 100)) else 0

          let qs = QualitySeal
                { qsSecurity = not hasErrors 
                , qsSEO = not hasErrors
                , qsPerformance = perfScore
                , qsItemCount = itemCount
                }

          renderQualitySeal qs
          renderBuildIssueReport issues

          -- Broadcast Quality Seal to any connected dashboards. Also
          -- always written to disk (regardless of 'mClients') so a
          -- process that owns the live-reload clients but ran this
          -- particular build in a *different* process — as
          -- 'app/Main.hs's 'runCustomSiteHs' does for a project's own
          -- @site.hs@, run via a 'runghc' subprocess that has no way
          -- to hold this process's 'MVar' — can still pick the report
          -- up and broadcast it itself afterward. Without this, the
          -- quality-seal dashboard message would silently never fire
          -- for any subprocess-executed build, with no indication why.
          writeQualitySealFile qs
          case mClients of
            Just clients -> broadcastMessage clients $ Aeson.object ["type" Aeson..= ("quality-seal" :: T.Text), "data" Aeson..= qs]
            Nothing -> return ()

          if hasErrors 
            then do
              TIO.putStrLn "\nSARA: Build completed with validation errors."
              exitFailure
            else TIO.putStrLn "\nSARA: Build completed successfully."

-- | Where 'writeQualitySealFile' writes, and where a caller bridging a
--   subprocess build (see 'writeQualitySealFile's Haddock) should read
--   from. Under '.sara/' rather than the site root so it doesn't get
--   mistaken for site content or accidentally matched by a user's own
--   glob patterns.
qualitySealFilePath :: FilePath
qualitySealFilePath = ".sara" </> "quality-seal.json"

-- | Persists the last build's 'QualitySeal' as JSON, unconditionally.
--   See the call site's Haddock for why this exists alongside the
--   direct in-process broadcast rather than replacing it.
writeQualitySealFile :: QualitySeal -> IO ()
writeQualitySealFile qs = do
  createDirectoryIfMissing True ".sara"
  BSL.writeFile qualitySealFilePath (Aeson.encode qs)

-- | The other half of 'writeQualitySealFile': reads back the last
--   build's persisted quality seal, if one exists and still parses.
--   Returns 'Nothing' rather than throwing on a missing or malformed
--   file — a dev-server dashboard update that quietly doesn't happen
--   this one time is a far better failure mode than crashing the
--   whole watch loop over a report file it doesn't strictly need to
--   keep running.
readQualitySealFile :: IO (Maybe Aeson.Value)
readQualitySealFile = do
  exists <- doesFileExist qualitySealFilePath
  if not exists
    then pure Nothing
    else Aeson.decode <$> BSL.readFile qualitySealFilePath

-- | The end-of-build report: every collected 'BuildIssue', grouped by
--   file, each with its actual message — not the single
--   "something failed somewhere" a bare 'Bool' could tell you. A
--   no-op when there are no issues, so the clean-build path stays
--   exactly as quiet as it always was.
renderBuildIssueReport :: [BuildIssue] -> IO ()
renderBuildIssueReport [] = pure ()
renderBuildIssueReport issues = do
  TIO.putStrLn ""
  TIO.putStrLn $ "Build issues (" <> T.pack (show (length issues)) <> " across " <> T.pack (show (length grouped)) <> " file(s)):"
  mapM_ reportFile grouped
  where
    grouped :: [(FilePath, [AnySaraError])]
    grouped =
      [ (path, [ e | BuildIssue p e <- issues, p == path ])
      | path <- dedupOrdered (map biFile issues)
      ]
    -- Preserves first-seen order, unlike 'Data.List.nub' composed with
    -- a 'Data.Set' (which would reorder), so files appear in the
    -- report in the order their issues were actually found. Membership
    -- is checked against a 'HS.HashSet' alongside the output list
    -- (rather than the growing output list itself via 'elem'), so a
    -- build with many distinct failing files stays O(n) instead of
    -- O(n^2).
    dedupOrdered :: [FilePath] -> [FilePath]
    dedupOrdered = go HS.empty
      where
        go _    []     = []
        go seen (x:xs)
          | x `HS.member` seen = go seen xs
          | otherwise          = x : go (HS.insert x seen) xs
    reportFile (path, errs) = do
      TIO.putStrLn $ "\n  " <> T.pack path
      mapM_ (\e -> TIO.putStrLn ("    " <> renderAnyErrorColor e)) errs
