{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import SARA
import SARA.Migration.Detect
import SARA.Migration.Scaffold
import SARA.Migration.Hakyll
import SARA.Migration.Jekyll (migrateJekyllPosts)
import SARA.Migration.Hugo (migrateHugoContent)
import SARA.LiveReload.Server
import SARA.LiveReload.Watcher
import SARA.FileSystem (listFilesRecursiveFrom)
import qualified Network.Wai.Handler.Warp as Warp
import Control.Concurrent (forkIO, MVar)
import Control.Monad (forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, doesFileExist, findExecutable)
import System.FilePath ((</>), takeExtension, makeRelative)
import System.Environment (withArgs, getArgs, getEnvironment)
import System.Process (proc, createProcess, waitForProcess, std_in, env, StdStream(..), readProcessWithExitCode)
import System.Timeout (timeout)
import System.Exit (ExitCode(..), exitFailure)
import Control.Exception (catch, IOException)
import Data.List (isInfixOf)

data BuildOpts = BuildOpts { bldDryRun :: !Bool, bldCacheKey :: !Bool }
data ServeOpts = ServeOpts { srvPort :: !Int }

data Command
  = Build !BuildOpts
  | Serve !ServeOpts
  | Import !FilePath
  | New !(Maybe FilePath) !(Maybe FilePath)
  | Check

main :: IO ()
main = do
  args <- getArgs
  if null args 
    then runDefaultBuild Nothing False
    else do
      let parser = info (helper <*> commandParser) 
                        (fullDesc <> progDesc "SARA: Simple, Adaptive, Responsive Architecture")
      cmd <- execParser parser
      case cmd of
        Build bOpts
          | bldCacheKey bOpts -> runCacheKey
          | otherwise         -> runDefaultBuild Nothing (bldDryRun bOpts)
        Serve sOpts  -> runServe (srvPort sOpts)
        Import path  -> runImport path
        New maybePath maybeTpl -> runNew maybePath maybeTpl
        Check        -> runCheck

runDefaultBuild :: Maybe (MVar ClientList) -> Bool -> IO ()
runDefaultBuild mClients dryRun = do
  if dryRun
    then putStrLn "SARA: Running dry run (no files will be written)..."
    else putStrLn "SARA: Running industrial build..."

  hasSiteHs <- doesFileExist "site.hs"
  if hasSiteHs
    then runCustomSiteHs dryRun
    else putStrLn "SARA: No site.hs found. Using zero-config defaults..." >> withArgs [] (saraWithOptions mClients dryRun defaultSite)

-- | Directory used to cache a compiled @site.hs@ binary between runs
--   — see 'ensureSiteCompiled'.
siteCacheDir :: FilePath
siteCacheDir = ".sara" </> "site-cache"

siteCacheBinaryPath :: FilePath
siteCacheBinaryPath = siteCacheDir </> "site-exe"

siteCacheKeyPath :: FilePath
siteCacheKeyPath = siteCacheDir </> "cache-key.txt"

-- | Compiles and runs a project's own @site.hs@ — compiling it once
--   and reusing that compiled binary on every subsequent run as long
--   as neither @site.hs@ (nor any sibling @.hs@ file it might import,
--   nor the active GHC version) has changed since. This used to
--   re-interpret @site.hs@ from scratch via @runghc@ on every single
--   build, including every file-watch-triggered rebuild during 'sara
--   serve' — paying full parse + typecheck + interpret cost on every
--   save even when only a markdown file changed and @site.hs@ itself
--   was completely untouched. See BUILD_AND_FIXES_SUMMARY.md's
--   engineering roadmap, item #1.
--
--   This also happens to be the other half of item #2's story
--   (parallel Shake builds): GHC's threaded RTS, which parallel
--   builds need, is unavailable to interpreted code — @runghc@
--   rejects @-N@ RTS flags outright — so compiling @site.hs@ is a
--   prerequisite for a real project's build to benefit from
--   parallelism at all, not just an independent speedup of its own.
--
--   A bounded timeout still guards *running* the compiled binary
--   (mirroring the previous @runghc@ timeout's intent): a genuine
--   infinite loop in a project's own build logic is exactly as
--   possible in compiled code as interpreted, so that risk doesn't go
--   away just because compilation itself can no longer hang the way
--   @runghc@ package-resolution apparently could.
--
--   '--dry-run' is threaded through via the @SARA_DRY_RUN@ environment
--   variable (see 'SARA.sara's Haddock) rather than a command-line
--   argument, since @site.hs@'s own @main@ decides what arguments (if
--   any) it parses — this codebase can't inject a flag into code it
--   doesn't control the entry point of, but every 'sara'-based
--   @site.hs@ already reads this one environment variable for free.
runCustomSiteHs :: Bool -> IO ()
runCustomSiteHs dryRun = do
  compiled <- ensureSiteCompiled
  case compiled of
    Left err -> putStrLn err >> exitFailure
    Right binPath -> runCompiledSite binPath dryRun

-- | Ensures a compiled, up-to-date @site.hs@ binary exists at
--   'siteCacheBinaryPath', compiling fresh only when the cache key
--   (hash of every @.hs@ file under the project, plus the active GHC
--   version) differs from the one recorded for whatever binary is
--   already cached there.
ensureSiteCompiled :: IO (Either String FilePath)
ensureSiteCompiled = do
  cwd <- getCurrentDirectory
  createDirectoryIfMissing True siteCacheDir
  currentKey <- computeCacheKey cwd
  cachedKey <- readCachedKey
  binaryExists <- doesFileExist siteCacheBinaryPath
  if binaryExists && cachedKey == Just currentKey
    then pure (Right siteCacheBinaryPath)
    else compileSite currentKey

computeCacheKey :: FilePath -> IO T.Text
computeCacheKey cwd = do
  scriptKey <- siteScriptCacheKey cwd
  mGhc <- findExecutable "ghc"
  ghcVersion <- case mGhc of
    Nothing -> pure "no-ghc"
    Just ghcPath -> do
      (exitCode, out, _) <- readProcessWithExitCode ghcPath ["--numeric-version"] ""
      pure $ case exitCode of
        ExitSuccess -> filter (/= '\n') out
        ExitFailure _ -> "unknown-ghc-version"
  pure (scriptKey <> ":" <> T.pack ghcVersion)

readCachedKey :: IO (Maybe T.Text)
readCachedKey = do
  exists <- doesFileExist siteCacheKeyPath
  if not exists
    then pure Nothing
    else (Just <$> TIO.readFile siteCacheKeyPath) `catch` \(_ :: IOException) -> pure Nothing

-- | Compile @site.hs@ (in the current directory) to
--   'siteCacheBinaryPath', recording @cacheKey@ alongside it on
--   success so the next 'ensureSiteCompiled' call can skip this step
--   entirely if nothing relevant has changed.
compileSite :: T.Text -> IO (Either String FilePath)
compileSite cacheKey = do
  mGhc <- findExecutable "ghc"
  case mGhc of
    Nothing -> pure $ Left "SARA: 'ghc' was not found on PATH — it ships with GHC, so this usually means GHC isn't installed or isn't on PATH."
    Just ghcPath -> do
      putStrLn "SARA: Compiling site.hs (first run, or site.hs changed since the last build)..."
      (exitCode, _, errOutput) <- readProcessWithExitCode ghcPath
        [ "-O0", "--make", "site.hs"
        , "-o", siteCacheBinaryPath
        , "-odir", siteCacheDir
        , "-hidir", siteCacheDir
        ] ""
      case exitCode of
        ExitSuccess -> do
          TIO.writeFile siteCacheKeyPath cacheKey
          pure (Right siteCacheBinaryPath)
        ExitFailure _
          | "Could not find module" `isInfixOf` errOutput && "SARA" `isInfixOf` errOutput ->
              pure $ Left $ unlines
                [ "SARA: The 'sara' library isn't visible to 'ghc' yet."
                , "      Fix: run 'cabal install --lib .' from the SARA source tree (or 'cabal install --lib sara' once it's published), then retry."
                ]
          | otherwise ->
              pure $ Left $ unlines
                [ "SARA: site.hs failed to compile:"
                , errOutput
                ]

-- | Run an already-compiled @site.hs@ binary, under the same bounded
--   timeout and dry-run environment-variable handling the previous
--   @runghc@-based implementation used.
runCompiledSite :: FilePath -> Bool -> IO ()
runCompiledSite binPath dryRun = do
  baseEnv <- getEnvironment
  let procEnv = if dryRun then ("SARA_DRY_RUN", "1") : baseEnv else baseEnv
  (_, _, _, ph) <- createProcess
    (proc binPath []) { std_in = NoStream, env = Just procEnv }
  result <- timeout (120 * 1000000) (waitForProcess ph)
  case result of
    Nothing -> do
      putStrLn "SARA: Timed out after 120s waiting for site.hs to build and run."
      putStrLn "      This is likely a genuine issue in site.hs itself (e.g. an infinite loop), not a missing-package problem."
      exitFailure
    Just ExitSuccess -> pure ()
    Just (ExitFailure code) -> do
      putStrLn $ "SARA: site.hs exited with an error (exit code " ++ show code ++ ")."
      exitFailure

-- | The default build pipeline used by the CLI.
--   Now includes automated Search Indexing and Asset discovery.
defaultSite :: SaraM ()
defaultSite = do
  assetsGlob <- glob "assets/*"
  discover assetsGlob
  postsGlob <- glob "posts/*.md"
  allPosts <- match postsGlob $ \file -> do
    item <- readMarkdown file
    validateSEO item

  -- Drafts (draft: true) and future-dated posts are excluded from a
  -- normal build by default, the same way every mainstream SSG
  -- behaves — see SARA.Content.Drafts. Filtering happens here, before
  -- 'render' is called, not after: rendering first and filtering the
  -- returned list afterward (as an earlier version of this pipeline's
  -- search-index step alone did) would still have written the draft's
  -- HTML to disk.
  posts <- filterPublished allPosts
  mapM_ (render "templates/post.html") posts

  -- Automatically generate search index if posts exist
  case posts of
    [] -> pure ()
    ps -> buildSearchIndex "search-index.json" ps

runServe :: Int -> IO ()
runServe port = do
  putStrLn $ "SARA: Starting dev server on port " ++ show port
  clients <- startLiveReloadServer
  curr <- getCurrentDirectory
  let siteDir = curr </> "_site"
  
  runDefaultBuild (Just clients) False
  broadcastQualitySealIfWritten clients
  
  -- Create site dir if it doesn't exist yet to avoid server error
  createDirectoryIfMissing True siteDir
  
  _ <- forkIO $ Warp.run port (liveReloadApp port clients siteDir)
  watchProject curr $ do
    runDefaultBuild (Just clients) False
    broadcastPatches clients siteDir
    broadcastQualitySealIfWritten clients

-- | Bridges the quality-seal dashboard broadcast across a
--   subprocess-executed @site.hs@ build. 'SARA.saraWithOptions'
--   already broadcasts this directly when it holds the live-reload
--   'MVar' itself (the zero-config 'defaultSite' path, run in-process)
--   — but 'runCustomSiteHs' runs a project's own @site.hs@ as a
--   separate 'runghc' subprocess, which has no way to hold this
--   process's 'MVar' at all. 'SARA.saraWithOptions' always persists
--   the report to '.sara/quality-seal.json' regardless of whether it
--   could broadcast directly (see its Haddock), so this reads that
--   back and broadcasts it here instead — the one bridge needed to
--   make the dashboard message fire the same way for both build
--   paths. A harmless no-op (via 'Nothing') if the file doesn't exist
--   or doesn't parse, so a dev-server session never crashes over a
--   report it doesn't strictly need to keep running.
broadcastQualitySealIfWritten :: MVar ClientList -> IO ()
broadcastQualitySealIfWritten clients = do
  mSeal <- readQualitySealFile
  case mSeal of
    Nothing -> pure ()
    Just sealValue -> broadcastMessage clients $
      Aeson.object ["type" Aeson..= ("quality-seal" :: T.Text), "data" Aeson..= sealValue]

broadcastPatches :: MVar ClientList -> FilePath -> IO ()
broadcastPatches clients siteDir = do
  files <- listFilesRecursive siteDir
  let htmlFiles = filter (\f -> takeExtension f == ".html") files
  forM_ htmlFiles $ \f -> do
    content <- TIO.readFile f
    let relPath = "/" ++ makeRelative siteDir f
    broadcastToPath clients (T.pack relPath) $ Aeson.object 
      [ "type" Aeson..= ("patch" :: T.Text)
      , "path" Aeson..= T.pack relPath
      , "html" Aeson..= content
      ]

-- | Full (not relative) paths, via the same recursive-walk helper
--   'SARA.Migration.Hugo.findMarkdownFilesRecursive' uses — the two
--   used to hand-roll the identical traversal independently.
listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = map (dir </>) <$> listFilesRecursiveFrom dir ""

runImport :: FilePath -> IO ()
runImport path = do
  putStrLn $ "Importing site from " ++ path
  ssg <- detectSourceSSG path
  case ssg of
    SourceJekyll -> do
      putStrLn "Detected Jekyll site. Configuring SARA..."
      scaffoldProject path (ScaffoldOptions "Migrated Jekyll Site" "Author" "/")
      (migrated, failed) <- migrateJekyllPosts path path
      reportContentMigration "Jekyll" migrated failed
    SourceHugo   -> do
      putStrLn "Detected Hugo site. Configuring SARA..."
      scaffoldProject path (ScaffoldOptions "Migrated Hugo Site" "Author" "/")
      (migrated, failed) <- migrateHugoContent path path
      reportContentMigration "Hugo" migrated failed
    SourceHakyll -> do
      putStrLn "Detected Hakyll site. Configuring SARA..."
      res <- migrateHakyllProject path
      case res of
        Left err -> print err
        Right msg -> do
          TIO.putStrLn msg
          scaffoldProject path (ScaffoldOptions "Migrated Hakyll Site" "Author" "/")
    SourceUnknown -> putStrLn "Unknown SSG format. Please see docs/MIGRATION.md"

-- | Prints an honest content-migration summary: every file that
--   migrated, every file that didn't (with the specific reason), and
--   an accurate total — never a bare "done" that could be hiding
--   silently-skipped or silently-corrupted files. 'scaffoldProject'
--   still runs first regardless (it only ever writes SARA's own
--   scaffold files, none of which depend on whether content migration
--   succeeds), so a person always gets a working, buildable project
--   even if every single post failed to migrate — they just get told
--   exactly that, with next steps, rather than a project that quietly
--   ships a placeholder post and no explanation of what happened to
--   their real content.
reportContentMigration :: String -> [FilePath] -> [(FilePath, e)] -> IO ()
reportContentMigration _ migrated [] | not (null migrated) =
  putStrLn $ "Migrated " ++ show (length migrated) ++ " post(s) successfully."
reportContentMigration ssgName [] [] =
  putStrLn $ "No " ++ ssgName ++ " content directory found (or it was empty) — nothing to migrate. The scaffolded project's sample post is still there to get you started."
reportContentMigration ssgName migrated failed = do
  putStrLn $ "Migrated " ++ show (length migrated) ++ " post(s) successfully."
  putStrLn $ show (length failed) ++ " post(s) could not be migrated automatically and were left untouched (not partially translated):"
  mapM_ (\(f, _) -> putStrLn ("  - " ++ f)) failed
  putStrLn $ "Review these " ++ ssgName ++ " files manually; each one has an unclosed or malformed tag SARA couldn't safely translate."

runNew :: Maybe FilePath -> Maybe FilePath -> IO ()
runNew maybePath maybeTpl = do
  let name = case maybePath of
               Just n -> n
               Nothing -> "my-sara-site"
  curr <- getCurrentDirectory
  let root = curr </> name
  case maybeTpl of
    Just tpl -> scaffoldFromTemplate tpl root (ScaffoldOptions (T.pack name) "Author" "/")
    Nothing  -> scaffoldProject root (ScaffoldOptions (T.pack name) "Author" "/")
  putStrLn $ "New SARA project created in " ++ root

runCheck :: IO ()
runCheck = do
  putStrLn "SARA: Checking site configuration and integrity..."
  -- Same fix as 'runDefaultBuild': this used to unconditionally call
  -- 'sara defaultSite', silently ignoring a project's own 'site.hs'
  -- exactly the way 'runDefaultBuild' used to — an identical bug,
  -- just never caught here because nothing had exercised 'sara check'
  -- against a project with a real 'site.hs' either. Treated as a dry
  -- run: a "check" should validate without writing, whichever
  -- pipeline it's checking.
  hasSiteHs <- doesFileExist "site.hs"
  if hasSiteHs
    then runCustomSiteHs True
    else withArgs [] $ saraWithOptions Nothing True defaultSite

runCacheKey :: IO ()
runCacheKey = do
  cwd <- getCurrentDirectory
  key <- projectCacheKey cwd
  TIO.putStrLn key

commandParser :: Parser Command
commandParser = subparser
  (  command "build"  (info (Build <$> (BuildOpts <$> switch (long "dry-run" <> help "Perform a dry run")
                                                    <*> switch (long "cache-key" <> help "Print a deterministic hash of the project's content/template tree and exit, without building. Suitable for use directly as a CI cache key.")))
                            (progDesc "Build the site"))
  <> command "serve"  (info (Serve . ServeOpts <$> option auto (long "port" <> short 'p' <> value 8080 <> help "Port to serve on")) (progDesc "Start development server"))
  <> command "import" (info (Import <$> strArgument (metavar "PATH")) (progDesc "Import existing site"))
  <> command "new"    (info (New <$> optional (strArgument (metavar "NAME")) <*> optional (strOption (long "template" <> short 't' <> metavar "PATH" <> help "Template directory"))) (progDesc "Create new project"))
  <> command "check"  (info (pure Check)  (progDesc "Validate site"))
  )
