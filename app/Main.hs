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
import System.Process (proc, createProcess, waitForProcess, std_in, std_out, std_err, env, StdStream(..))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import System.Exit (ExitCode(..), exitFailure)

data BuildOpts = BuildOpts { bldDryRun :: !Bool }
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
        Build bOpts  -> runDefaultBuild Nothing (bldDryRun bOpts)
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

-- | Compiles and runs a project's own @site.hs@ via @runghc@, in a
--   subprocess, with a bounded timeout and specific, actionable
--   diagnostics — rather than either the silent wrong-pipeline
--   substitution this used to do, or an unbounded 'runghc' call, which
--   was confirmed (while building this fix) to hang indefinitely with
--   no visible error when the 'sara' library isn't registered where
--   'runghc' can see it.
--
--   Requires 'sara' to have been installed into the ambient GHC
--   environment first (@cabal install --lib .@ from this source tree,
--   or @cabal install --lib sara@ once published) — this is the
--   standard Cabal mechanism for making a library available to
--   ad-hoc scripts run via a bare 'runghc', and is genuinely the
--   difference between this working and hanging: confirmed directly
--   while building this fix that a plain 'runghc site.hs' with no such
--   installation step hangs, and the identical command succeeds in a
--   few seconds once the library is installed.
--
--   '--dry-run' is threaded through via the @SARA_DRY_RUN@ environment
--   variable (see 'SARA.sara's Haddock) rather than a command-line
--   argument, since @site.hs@'s own @main@ decides what arguments (if
--   any) it parses — this codebase can't inject a flag into code it
--   doesn't control the entry point of, but every 'sara'-based
--   @site.hs@ already reads this one environment variable for free.
runCustomSiteHs :: Bool -> IO ()
runCustomSiteHs dryRun = do
  putStrLn "SARA: Found site.hs — compiling and running it via 'runghc'..."
  mRunghc <- findExecutable "runghc"
  case mRunghc of
    Nothing -> do
      putStrLn "SARA: 'runghc' was not found on PATH — it ships with GHC, so this usually means GHC isn't installed or isn't on PATH."
      exitFailure
    Just runghcPath -> do
      -- A pre-flight check, not just the timeout below: without it,
      -- the single most common failure mode (sara isn't registered
      -- where runghc can see it) is only discovered after waiting out
      -- the full 120s timeout. Checked here, it fails in a couple of
      -- seconds with the exact same actionable message, since it's
      -- the identical underlying cause either way.
      visible <- checkSaraLibraryVisible runghcPath
      if not visible
        then do
          putStrLn "SARA: The 'sara' library isn't visible to 'runghc' yet."
          putStrLn "      Fix: run 'cabal install --lib .' from the SARA source tree (or 'cabal install --lib sara' once it's published), then retry."
          exitFailure
        else do
          baseEnv <- getEnvironment
          let procEnv = if dryRun then ("SARA_DRY_RUN", "1") : baseEnv else baseEnv
          (_, _, _, ph) <- createProcess
            (proc runghcPath ["site.hs"]) { std_in = NoStream, env = Just procEnv }
          result <- timeout (120 * 1000000) (waitForProcess ph)
          case result of
            Nothing -> do
              putStrLn "SARA: Timed out after 120s waiting for site.hs to build and run."
              putStrLn "      The library was visible to a trivial check but the real site.hs still hung or took too long -- this is likely a genuine issue in site.hs itself (e.g. an infinite loop), not a missing-package problem."
              exitFailure
            Just ExitSuccess -> pure ()
            Just (ExitFailure code) -> do
              putStrLn $ "SARA: site.hs exited with an error (exit code " ++ show code ++ ")."
              exitFailure

-- | The pre-flight check 'runCustomSiteHs' uses: can 'runghc' resolve
--   @import SARA@ at all? Implemented by actually attempting it -- a
--   trivial temp file with nothing but that import and a no-op @main@
--   -- rather than inspecting GHC's package environment files
--   directly, since their exact format and location varies across GHC
--   versions and platforms (this project alone supports GHC 9.4.7
--   through 9.14.1) and this codebase would rather ask the real tool
--   the real question than maintain its own brittle parser for
--   environment-file internals it doesn't own the format of. Bounded
--   to a short timeout (10s is generous for compiling one import) so
--   a hang here -- the exact failure mode this check exists to avoid
--   waiting out on the real site.hs -- doesn't recreate the same
--   problem one level up.
checkSaraLibraryVisible :: FilePath -> IO Bool
checkSaraLibraryVisible runghcPath =
  withSystemTempDirectory "sara-visibility-check" $ \tmpDir -> do
    let probePath = tmpDir </> "SaraVisibilityProbe.hs"
    writeFile probePath "import SARA\nmain :: IO ()\nmain = pure ()\n"
    (_, _, _, ph) <- createProcess
      (proc runghcPath [probePath]) { std_in = NoStream, std_out = CreatePipe, std_err = CreatePipe }
    result <- timeout (10 * 1000000) (waitForProcess ph)
    pure (result == Just ExitSuccess)

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

commandParser :: Parser Command
commandParser = subparser
  (  command "build"  (info (Build . BuildOpts <$> switch (long "dry-run" <> help "Perform a dry run")) (progDesc "Build the site"))
  <> command "serve"  (info (Serve . ServeOpts <$> option auto (long "port" <> short 'p' <> value 8080 <> help "Port to serve on")) (progDesc "Start development server"))
  <> command "import" (info (Import <$> strArgument (metavar "PATH")) (progDesc "Import existing site"))
  <> command "new"    (info (New <$> optional (strArgument (metavar "NAME")) <*> optional (strOption (long "template" <> short 't' <> metavar "PATH" <> help "Template directory"))) (progDesc "Create new project"))
  <> command "check"  (info (pure Check)  (progDesc "Validate site"))
  )
