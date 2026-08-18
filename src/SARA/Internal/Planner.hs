{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module SARA.Internal.Planner
  ( planRules
  , collectOutputs
  , expandRules
  ) where

import Development.Shake
import Development.Shake.FilePath
import SARA.Monad (RuleDecl(..), SaraEnv(..), SaraM(..), BuildIssue(..))
import SARA.Types (Item, ItemP(..), AssetKind(..), SomeAssetKind(..), GlobPattern, ValidationState(..), FeedConfig(..))
import SARA.Config (SaraConfig(..))
import SARA.Security.PathGuard (guardPath, unSafePath)
import SARA.Security.GlobGuard (unGlobPattern)
import SARA.Template.Renderer (renderTemplate)
import SARA.Internal.Hash (needBlake3, askLQIP)
import SARA.Error (AnySaraError(..), SaraBuildException(..), renderAnyErrorColor)
import SARA.SEO.Audit (auditRenderedHTML, AuditResult(..))
import SARA.Validator.LinkChecker (checkInternalLinks)
import SARA.Asset.Discover (inferAssetKind)
import SARA.Asset.Image (processImage)
import SARA.Search.Index (generatePartialIndex, mergePartialIndexes, mkSearchEntry)
import SARA.SEO.Sitemap (generateSitemap)
import SARA.SEO.Feed (generateRSS)
import Data.Maybe (mapMaybe)

import Control.Monad (forM_)
import Control.Exception (throwIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as BS
import Data.IORef (atomicModifyIORef')
import System.IO (hFlush, stdout)
import System.FilePath.Glob (globDir1, compile)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

collectOutputs :: SaraEnv -> [RuleDecl] -> [FilePath]
collectOutputs env decls = mapMaybe declOutput decls
  where
    outDir = cfgOutputDirectory (envConfig env)
    -- Normalised so a path carrying a literal "./" segment (as
    -- 'glob'-matched paths do, e.g. "./posts/x.md") produces the
    -- same site-graph key an absolute-href lookup in
    -- 'SARA.Validator.LinkChecker' resolves to — both sides must
    -- agree on one canonical form, not just "however '</>' happened
    -- to concatenate them."
    mkOutput p = normalise (outDir </> p)
    -- Each 'RuleDecl' independently maps to zero or one output paths —
    -- no state threads between elements — so this is 'mapMaybe' over a
    -- per-constructor decision rather than hand-written recursion.
    declOutput = \case
      RuleRender _ _ outPath       -> Just (mkOutput outPath)
      RuleRenderRaw _ _ outPath    -> Just (mkOutput outPath)
      RuleDiscover _               -> Nothing
      RuleRemap _                  -> Nothing
      RuleSearch outPath _         -> Just (mkOutput outPath)
      RulePartialSearch outPath _  -> Just (normalise (outDir </> ".cache" </> outPath))
      RuleSitemap outPath _        -> Just (mkOutput outPath)
      RuleRSS outPath _ _          -> Just (mkOutput outPath)
      RuleGlobal _                 -> Nothing

-- | Translate RuleDecls from DSL into Shake Rules.
planRules :: SaraEnv -> [RuleDecl] -> Rules ()
planRules env decls = do
  -- Collect all output paths from decls to create a 'want' rule.
  --
  -- 'collectOutputs' deliberately excludes 'RuleDiscover' outputs (see
  -- its Haddock and 'expandRules's) — discovered assets aren't
  -- validated the way pages are, so they're intentionally not part of
  -- the dry-run report or the site graph. But that same exclusion,
  -- applied here too, was a real, severe bug: nothing else in a
  -- typical site depends on a discovered asset as a build input, so
  -- with no entry in 'want' at all, 'genDiscover's '%>' rule for it
  -- was registered but never actually triggered — a discovered image
  -- or stylesheet was silently never copied to the output directory,
  -- confirmed directly by a real end-to-end test that checked for the
  -- file's actual presence rather than assuming a rule existing meant
  -- it ran. Fixed by explicitly globbing each 'RuleDiscover' pattern
  -- here too and 'want'-ing those paths — separately from
  -- 'collectOutputs', so the dry-run/site-graph exclusion above stays
  -- exactly as documented and doesn't need to change to fix this.
  let allOutputs = collectOutputs env decls
  discoveredOutputs <- fmap concat . liftIO $ mapM (discoverOutputPaths env) [ g | RuleDiscover g <- decls ]
  want (allOutputs ++ discoveredOutputs)

  -- Default rule to ensure output directory exists
  let outDir = cfgOutputDirectory (envConfig env)
  outDir %> \out -> do
    liftIO $ createDirectoryIfMissing True out

  mapM_ (translateDecl env) decls

-- | The output path every file matching a 'discover' glob will be
--   copied (or, for images, processed) to — the same computation
--   'genDiscover' does per-file when it installs each '%>' rule,
--   duplicated here (rather than shared) because this one only needs
--   the resulting *paths*, not to install anything; see 'planRules'
--   for why this exists at all.
discoverOutputPaths :: SaraEnv -> GlobPattern -> IO [FilePath]
discoverOutputPaths env g = do
  let patStr = T.unpack (unGlobPattern g)
  files <- globDir1 (compile patStr) "."
  let outDir = cfgOutputDirectory (envConfig env)
  pure [ outDir </> f | f <- files ]

translateDecl :: SaraEnv -> RuleDecl -> Rules ()
translateDecl env = \case
  RuleDiscover g       -> genDiscover env g
  RuleRender t i o     -> genRender env t i o
  RuleRenderRaw h i o  -> genRenderRaw env h i o
  RuleRemap _          -> return ()
  RuleSearch o items   -> genSearch env o items
  RulePartialSearch o i -> genPartialSearch env o i
  RuleSitemap o items  -> genSitemap env o items
  RuleRSS o cfg items  -> genRSS env o cfg items
  RuleGlobal globalAction -> genGlobal env globalAction

-- | Recursively expand every 'RuleGlobal' in a decl list into the
--   concrete decls (RuleRender, RuleSearch, ...) it produces, by
--   actually running the nested 'SaraM' action — the same work
--   'genGlobal' does inside the Shake monad, but as plain 'IO' so it
--   can run outside of Shake entirely.
--
--   This exists for 'SARA.Internal.Engine.previewBuild' (dry-run):
--   without it, a dry run would only see the literal top-level
--   'RuleGlobal' declaration and miss whatever concrete decls its
--   nested action produces. This is a deliberately separate,
--   read-only implementation from 'genGlobal' (0.6 of the Haskell
--   Engineering Standard: they look similar but serve different
--   concerns — one installs live Shake rules, this one only reports
--   what would happen).
--
--   'match' no longer needs an entry here: it runs eagerly, in 'SaraM'
--   itself, at the point it's called (see its Haddock in "SARA.DSL"
--   for why — this used to be a 'RuleMatch' declaration expanded only
--   here or in 'genMatch', and that deferred-expansion design was the
--   root cause of a real bug where 'match' always returned @[]@
--   regardless of what it matched). By the time any 'RuleDecl' list
--   reaches this function, every file 'match' would have touched has
--   already been processed — there is nothing left to expand for it.
--
--   'RuleDiscover' is intentionally left unexpanded here, consistent
--   with 'collectOutputs', which likewise does not enumerate discovered
--   asset paths (they're addressed by their own Shake wildcard rule,
--   not by an upfront output list) — a dry run therefore does not
--   currently report planned asset copies, matching the existing,
--   narrower meaning of the word "outputs" this codebase's 'want' list
--   already uses.
expandRules :: SaraEnv -> [RuleDecl] -> IO [RuleDecl]
expandRules env = fmap concat . mapM (expandRuleDecl env)

expandRuleDecl :: SaraEnv -> RuleDecl -> IO [RuleDecl]
expandRuleDecl env = \case
  RuleGlobal globalAction ->
    runExceptT (runWriterT (runReaderT (unSaraM globalAction) env)) >>= \case
      Left errs -> do
        mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
        pure []
      Right ((), nestedDecls) -> expandRules env nestedDecls
  other -> pure [other]

genDiscover :: SaraEnv -> GlobPattern -> Rules ()
genDiscover env g = do
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  let outDir = cfgOutputDirectory (envConfig env)
  forM_ files $ \src -> do
    guarded <- liftIO $ guardPath (envRoot env) src
    case guarded of
      Left err -> liftIO $ TIO.putStrLn (renderAnyErrorColor (AnySaraError err))
      Right safeSrc -> do
        let out = outDir </> src
        case inferAssetKind src of
          SomeAssetKind (ImageAsset spec) -> do
            out %> \o -> do
              needBlake3 [unSafePath safeSrc]
              issues <- processImage spec safeSrc o
              liftIO $ recordBuildIssues env src issues
          _ -> do
            out %> \o -> do
              needBlake3 [unSafePath safeSrc]
              copyFile' (unSafePath safeSrc) o

-- | Writes rendered output as UTF-8 bytes explicitly, rather than
--   Shake's own 'Development.Shake.writeFile'' (which just calls
--   'System.IO.writeFile', encoding via whatever the process locale
--   happens to be). Real content — including this codebase's own
--   'SARA.Content.Summary.plainTextExcerpt', which can emit a literal
--   "…" — reliably contains non-ASCII characters; a build must not
--   crash on a system whose default locale isn't UTF-8, which a bare
--   'writeFile' does (confirmed directly: a plain 'runghc site.hs'
--   with no 'LANG' set fails with "cannot encode character '\8230'"
--   on exactly this ellipsis). Encoding explicitly makes correctness
--   independent of the invoking environment's locale, matching how
--   this codebase's own frontmatter/template reads already require
--   'LANG=C.UTF-8' in CI for the same underlying reason — the fix
--   belongs on the write side, not as an environment requirement
--   passed on to every user.
writeUtf8File :: FilePath -> Text -> Action ()
writeUtf8File path content = liftIO $ do
  createDirectoryIfMissing True (takeDirectory path)
  BS.writeFile path (TE.encodeUtf8 content)

genRender :: SaraEnv -> FilePath -> Item 'Validated -> FilePath -> Rules ()
genRender env tplPath item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath

  fullOutPath %> \o -> do
    liftIO $ do
      putStr $ "\r\ESC[2K  [RENDERING] " ++ outPath
      hFlush stdout
    guardedSrc <- liftIO $ guardPath (envRoot env) (itemPath item)
    case guardedSrc of
      Left err -> liftIO $ throwIO (SaraBuildException (renderAnyErrorColor (AnySaraError err)))
      Right safeSrc -> do
        guardedTpl <- liftIO $ guardPath (envRoot env) tplPath
        case guardedTpl of
          Left err -> liftIO $ throwIO (SaraBuildException (renderAnyErrorColor (AnySaraError err)))
          Right safeTpl -> do
            needBlake3 [unSafePath safeSrc, unSafePath safeTpl]
            
            let config = envConfig env
            let siteMeta = KM.fromList
                  [ (K.fromText "siteTitle", Aeson.String (cfgSiteTitle config))
                  , (K.fromText "siteUrl",   Aeson.String (cfgSiteUrl config))
                  , (K.fromText "siteAuthor", Aeson.String (cfgSiteAuthor config))
                  ]
            let itemWithBody = KM.insert (K.fromText "itemBody") (Aeson.String (itemBody item)) (itemMeta item)
            let ctx = Aeson.Object $ KM.union itemWithBody siteMeta
            
            renderTemplate (unSafePath safeTpl) ctx >>= \case
              Left err -> liftIO $ throwIO (SaraBuildException (T.pack (show err)))
              Right html -> do
                -- Inject real LQIPs by scanning for magic tokens
                finalHtml <- injectLQIPs html
                
                let sg = envSiteGraph env
                let linkIssues = checkInternalLinks outDir sg (itemPath item) outPath finalHtml
                let seoResult = auditRenderedHTML outPath finalHtml
                let allIssues = case seoResult of
                      AuditIssues _ issues -> issues ++ linkIssues
                      AuditPassed           -> linkIssues
                liftIO $ recordBuildIssues env outPath allIssues
                writeUtf8File o finalHtml

-- | Record every issue found while rendering one output file, tagged
--   with that file's path. A no-op if 'issues' is empty, so call sites
--   don't need their own guard for the clean case.
recordBuildIssues :: SaraEnv -> FilePath -> [AnySaraError] -> IO ()
recordBuildIssues _   _    []     = pure ()
recordBuildIssues env path issues =
  atomicModifyIORef' (envBuildIssues env) $ \existing ->
    (map (BuildIssue path) issues ++ existing, ())

genRenderRaw :: SaraEnv -> Text -> Item 'Validated -> FilePath -> Rules ()
genRenderRaw env html item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath

  fullOutPath %> \o -> do
    -- Inject real LQIPs even for raw HTML
    finalHtml <- injectLQIPs html
    
    let sg = envSiteGraph env
    let linkIssues = checkInternalLinks outDir sg (itemPath item) outPath finalHtml
    let seoResult = auditRenderedHTML outPath finalHtml
    let allIssues = case seoResult of
          AuditIssues _ issues -> issues ++ linkIssues
          AuditPassed           -> linkIssues
    liftIO $ recordBuildIssues env outPath allIssues
    writeUtf8File o finalHtml

injectLQIPs :: Text -> Action Text
injectLQIPs html = do
  let tokens = findLQIPTokens html
  replacements <- mapM (\t -> (t,) <$> askLQIP (T.unpack t)) tokens
  return $ foldr (\(token, b64) acc -> T.replace ("__LQIP__:" <> token <> "__") b64 acc) html replacements

findLQIPTokens :: Text -> [Text]
findLQIPTokens t = 
  let (_, match) = T.breakOn "__LQIP__:" t
  in if T.null match
     then []
     else 
       let rest = T.drop 8 match
           (path, after) = T.breakOn "__" rest
       in path : findLQIPTokens (T.drop 2 after)

genSearch :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSearch env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  
  let partials = map (\i -> outDir </> ".cache" </> (itemPath i ++ ".partial.json")) items
  forM_ (zip items partials) $ \(item, _) -> do
    genPartialSearch env (itemPath item ++ ".partial.json") item

  fullOutPath %> \o -> do
    need partials
    mergePartialIndexes partials o

genPartialSearch :: SaraEnv -> FilePath -> Item 'Validated -> Rules ()
genPartialSearch env outPath item = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> ".cache" </> outPath
  
  fullOutPath %> \o -> do
    need [itemPath item]
    let (entry, content) = mkSearchEntry item
    generatePartialIndex entry content o

genSitemap :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSitemap env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  
  fullOutPath %> \o -> do
    let sources = map itemPath items
    need sources
    generateSitemap (cfgSiteUrl (envConfig env)) items o

genRSS :: SaraEnv -> FilePath -> FeedConfig -> [Item 'Validated] -> Rules ()
genRSS env outPath cfg items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  
  fullOutPath %> \o -> do
    let sources = map itemPath items
    need sources
    generateRSS cfg items o

genGlobal :: SaraEnv -> SaraM () -> Rules ()
genGlobal env globalAction = do
  liftIO (runExceptT $ runWriterT $ runReaderT (unSaraM globalAction) env) >>= \case
    Left errs -> liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
    Right ((), nestedDecls) -> do
      let nestedOutputs = collectOutputs env nestedDecls
      want nestedOutputs
      mapM_ (translateDecl env) nestedDecls
