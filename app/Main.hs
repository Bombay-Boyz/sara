{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Options.Applicative
import SARA
import SARA.DSL (coerceToValidated)
import SARA.Migration.Detect (detectSourceSSG, SourceSSG(..))
import SARA.Migration.Scaffold
import SARA.Migration.Hakyll
import SARA.LiveReload.Server
import SARA.LiveReload.Watcher
import qualified Network.Wai.Handler.Warp as Warp
import Control.Concurrent (forkIO)
import System.Directory (getCurrentDirectory, listDirectory, doesDirectoryExist, createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (forM, forM_, void)
import UnliftIO.IORef
import UnliftIO.MVar
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import System.Environment (withArgs, getArgs)
import System.Process (callProcess)

data Command
  = Build BuildOptions
  | Serve ServeOptions
  | Import FilePath
  | New (Maybe FilePath) (Maybe FilePath)
  | Check

data BuildOptions = BuildOptions
  { bldDryRun :: Bool
  , bldJobs   :: Maybe Int
  }

data ServeOptions = ServeOptions
  { srvPort :: Int
  }

main :: IO ()
main = do
  opts <- execParser optsWithHelp
  case opts of
    Build bOpts  -> runDefaultBuild Nothing (bldDryRun bOpts) (bldJobs bOpts)
    Serve sOpts  -> runServe (srvPort sOpts)
    Import path  -> runImport path
    New maybePath maybeTpl -> runNew maybePath maybeTpl
    Check        -> runCheck
  where
    optsWithHelp = info (helper <*> parseCommand)
      (fullDesc <> progDesc "SARA: Industrial Static Site Engine")

parseCommand :: Parser Command
parseCommand = subparser
  (  command "build" (info (Build <$> (BuildOptions <$> switch (long "dry-run" <> help "Perform a dry run") <*> optional (option auto (long "jobs" <> short 'j' <> metavar "N" <> help "Number of parallel jobs")))) (progDesc "Build the site"))
  <> command "serve" (info (Serve <$> (ServeOptions <$> option auto (long "port" <> short 'p' <> value 8000 <> help "Port to serve on"))) (progDesc "Start dev server"))
  <> command "import" (info (Import <$> strArgument (metavar "PATH" <> help "Source site directory")) (progDesc "Import from another SSG"))
  <> command "new"    (info (New <$> optional (strArgument (metavar "NAME")) <*> optional (strOption (long "template" <> short 't' <> metavar "PATH" <> help "Template directory"))) (progDesc "Create new project"))
  <> command "check"  (info (pure Check)  (progDesc "Validate site"))
  )

runDefaultBuild :: Maybe (MVar ClientList) -> Bool -> Maybe Int -> IO ()
runDefaultBuild mClients _dryRun maybeJobs = do
  putStrLn "SARA: Running industrial build..."
  
  hasSiteHs <- doesFileExist "site.hs"
  if hasSiteHs
    then do
      putStrLn "SARA: Compiling and running site.hs..."
      args <- getArgs
      -- Ensure site.hs can find the SARA library
      callProcess "runghc" ("site.hs" : filter (/= "build") args)
    else do
      putStrLn "SARA: No site.hs found. Using zero-config defaults..."
      -- Forward jobs count to sara
      let args = case maybeJobs of
                   Just n -> ["--jobs=" ++ show n]
                   Nothing -> []
      withArgs args (saraWithClients mClients defaultSite)

-- | The default build pipeline used by the CLI.
defaultSite :: SaraM ()
defaultSite = do
  discover (glob "assets/*")
  posts <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    _ <- validateSEO item
    render "templates/post.html" (coerceToValidated item)
    pure item
  
  case posts of
    [] -> pure ()
    ps -> buildSearchIndex "search-index.json" ps

runServe :: Int -> IO ()
runServe port = do
  putStrLn $ "SARA: Starting dev server on port " ++ show port
  clients <- startLiveReloadServer
  curr <- getCurrentDirectory
  let siteDir = curr </> "_site"
  
  runDefaultBuild (Just clients) False Nothing
  
  createDirectoryIfMissing True siteDir
  
  _ <- forkIO $ Warp.run port (liveReloadApp clients siteDir)
  watchProject curr $ do
    runDefaultBuild (Just clients) False Nothing
    broadcastPatches clients siteDir

broadcastPatches :: MVar ClientList -> FilePath -> IO ()
broadcastPatches clients _siteDir = do
  broadcastMessage clients $ Aeson.object ["type" Aeson..= ("reload" :: T.Text)]

runImport :: FilePath -> IO ()
runImport path = do
  putStrLn $ "Importing site from " ++ path
  ssg <- detectSourceSSG path
  case ssg of
    SourceHakyll -> void $ migrateHakyllProject path
    _      -> putStrLn "SARA: Automatic migration for this SSG is not yet implemented."

runNew :: Maybe FilePath -> Maybe FilePath -> IO ()
runNew name _tpl = do
  let projectName = case name of
        Just n -> n
        Nothing -> "sara-site"
  putStrLn $ "SARA: Creating new project " ++ projectName
  scaffoldProject projectName (ScaffoldOptions (T.pack projectName) "Author" "/")

runCheck :: IO ()
runCheck = do
  putStrLn "SARA: Running site check..."
  runDefaultBuild Nothing False Nothing
