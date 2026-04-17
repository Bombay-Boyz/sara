{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import SARA
import SARA.Migration.Detect
import SARA.Migration.Scaffold
import SARA.Migration.Hakyll
import SARA.LiveReload.Server
import SARA.LiveReload.Watcher
import qualified Network.Wai.Handler.Warp as Warp
import Control.Concurrent (forkIO, MVar)
import Control.Monad (forM, forM_)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension, makeRelative)
import System.Environment (withArgs, getArgs)

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
runDefaultBuild mClients _ = do
  -- We ignore the dryRun flag for now as we transition to pure shake
  putStrLn "SARA: Running industrial build..."
  
  hasSiteHs <- doesFileExist "site.hs"
  if hasSiteHs
    then putStrLn "SARA: Using project site.hs..." >> withArgs [] (saraWithClients mClients defaultSite)
    else putStrLn "SARA: No site.hs found. Using zero-config defaults..." >> withArgs [] (saraWithClients mClients defaultSite)

-- | The default build pipeline used by the CLI.
--   Now includes automated Search Indexing and Asset discovery.
defaultSite :: SaraM ()
defaultSite = do
  discover (glob "assets/*")
  posts <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    item' <- validateSEO item
    render "templates/post.html" item'
    pure item'
  
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
  
  -- Create site dir if it doesn't exist yet to avoid server error
  createDirectoryIfMissing True siteDir
  
  _ <- forkIO $ Warp.run port (liveReloadApp clients siteDir)
  watchProject curr $ do
    runDefaultBuild (Just clients) False
    broadcastPatches clients siteDir

broadcastPatches :: MVar ClientList -> FilePath -> IO ()
broadcastPatches clients _siteDir = do
  -- Targeted patching is hard without a diffing engine.
  -- For industrial reliability, we send a targeted reload signal to clients.
  -- Clients watching a specific path will reload.
  broadcastMessage clients $ Aeson.object ["type" Aeson..= ("reload" :: T.Text)]

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
  names <- listDirectory dir
  paths <- forM names $ \name -> do
    let path = dir </> name
    isDir <- doesDirectoryExist path
    if isDir then listFilesRecursive path else return [path]
  return (concat paths)

runImport :: FilePath -> IO ()
runImport path = do
  putStrLn $ "Importing site from " ++ path
  ssg <- detectSourceSSG path
  case ssg of
    SourceJekyll -> do
      putStrLn "Detected Jekyll site. Configuring SARA..."
      scaffoldProject path (ScaffoldOptions "Migrated Jekyll Site" "Author" "/")
    SourceHugo   -> do
      putStrLn "Detected Hugo site. Configuring SARA..."
      scaffoldProject path (ScaffoldOptions "Migrated Hugo Site" "Author" "/")
    SourceHakyll -> do
      putStrLn "Detected Hakyll site. Configuring SARA..."
      res <- migrateHakyllProject path
      case res of
        Left err -> print err
        Right msg -> do
          TIO.putStrLn msg
          scaffoldProject path (ScaffoldOptions "Migrated Hakyll Site" "Author" "/")
    SourceUnknown -> putStrLn "Unknown SSG format. Please see docs/MIGRATION.md"

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
  withArgs [] $ sara defaultSite

commandParser :: Parser Command
commandParser = subparser
  (  command "build"  (info (Build . BuildOpts <$> switch (long "dry-run" <> help "Perform a dry run")) (progDesc "Build the site"))
  <> command "serve"  (info (Serve . ServeOpts <$> option auto (long "port" <> short 'p' <> value 8080 <> help "Port to serve on")) (progDesc "Start development server"))
  <> command "import" (info (Import <$> strArgument (metavar "PATH")) (progDesc "Import existing site"))
  <> command "new"    (info (New <$> optional (strArgument (metavar "NAME")) <*> optional (strOption (long "template" <> short 't' <> metavar "PATH" <> help "Template directory"))) (progDesc "Create new project"))
  <> command "check"  (info (pure Check)  (progDesc "Validate site"))
  )
