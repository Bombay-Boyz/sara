{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

module Main (main) where

import Options.Applicative
import SARA
import SARA.Migration.Engine (runMigration)
import SARA.Migration.Scaffold (scaffoldProject, ScaffoldOptions(..))
import SARA.LiveReload.Server (startLiveReloadServer, LiveReloadState)
import SARA.LiveReload.Watcher
import System.Directory (getCurrentDirectory, createDirectoryIfMissing, doesFileExist, removeDirectoryRecursive)
import System.Process (callProcess)
import System.Environment (getArgs, withArgs)
import qualified UnliftIO.Exception as E
import qualified Data.Text as T

data Commands
  = Build (Maybe Int)
  | Serve Int (Maybe Int)
  | Init
  | Import FilePath
  | Check
  | Clean
  | Version

main :: IO ()
main = do
  opts <- execParser optsInfo
  case opts of
    Build maybeJobs -> runBuild' maybeJobs
    Serve port maybeJobs -> runServe port maybeJobs
    Init -> runInit
    Import path -> runImport path
    Check -> runCheck
    Clean -> runClean
    Version -> putStrLn "SARA version 0.1.0.0"

optsInfo :: ParserInfo Commands
optsInfo = info (commands <**> helper)
  ( fullDesc <> progDesc "SARA: Simple, Adaptive, Responsive Architecture SSG" )

commands :: Parser Commands
commands = subparser
  ( command "build" (info (Build <$> jobsOpt) (progDesc "Build the static site"))
 <> command "serve" (info (Serve <$> portOpt <*> jobsOpt) (progDesc "Start dev server with live reload"))
 <> command "init"  (info (pure Init) (progDesc "Initialize a new SARA project"))
 <> command "import" (info (Import <$> argument str (metavar "PATH")) (progDesc "Import from another SSG"))
 <> command "check" (info (pure Check) (progDesc "Audit the site for SEO and performance"))
 <> command "clean" (info (pure Clean) (progDesc "Remove _site and _build directories"))
 <> command "version" (info (pure Version) (progDesc "Show version"))
  )
  where
    portOpt = option auto (long "port" <> short 'p' <> value 8000 <> help "Port for dev server")
    jobsOpt = optional (option auto (long "jobs" <> short 'j' <> help "Number of parallel jobs"))

runBuild' :: Maybe Int -> IO ()
runBuild' maybeJobs = do
  -- 1. Check if site.hs exists
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
      withArgs [] (saraWithClientsAndJobs Nothing maybeJobs Nothing defaultSite)

-- | The default build pipeline used by the CLI.
defaultSite :: SaraM ()
defaultSite = do
  discover (glob "assets/*")
  posts <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    validated <- validateSEO item
    render "templates/post.html" validated
    pure validated
  
  case posts of
    [] -> pure ()
    ps -> buildSearchIndex "search-index.json" ps

runServe :: Int -> Maybe Int -> IO ()
runServe port maybeJobs = do
  putStrLn $ "SARA: Starting dev server on port " ++ show port
  clients <- startLiveReloadServer
  curr <- getCurrentDirectory
  
  runDefaultBuild (Just clients) False maybeJobs
  
  watchProject curr $ do
    runDefaultBuild (Just clients) True maybeJobs

runInit :: IO ()
runInit = do
  putStrLn "SARA: Initializing new project..."
  scaffoldProject "." (ScaffoldOptions "My Site" "SARA User" "http://localhost:8000")

runImport :: FilePath -> IO ()
runImport path = do
  putStrLn $ "SARA: Importing from " ++ path
  runMigration path "." >>= \case
    Right msg -> putStrLn $ "SARA: " ++ T.unpack msg
    Left err -> putStrLn $ "SARA Import Error: " ++ show err

runCheck :: IO ()
runCheck = do
  putStrLn "SARA: Auditing site (Dry Run)..."
  hasSiteHs <- doesFileExist "site.hs"
  if hasSiteHs
    then do
      args <- getArgs
      callProcess "runghc" ("site.hs" : "build" : "--dry-run" : filter (/= "check") args)
    else do
      withArgs [] (saraWithClientsAndJobs Nothing Nothing (Just (defaultConfig { cfgDryRun = True })) defaultSite)

runClean :: IO ()
runClean = do
  putStrLn "SARA: Cleaning project..."
  E.handle (\(_ :: E.SomeException) -> return ()) $ removeDirectoryRecursive "_site"
  E.handle (\(_ :: E.SomeException) -> return ()) $ removeDirectoryRecursive "_build"
  putStrLn "SARA: Clean complete."

runDefaultBuild :: Maybe LiveReloadState -> Bool -> Maybe Int -> IO ()
runDefaultBuild mClients _isIncremental maybeJobs = do
  createDirectoryIfMissing True "_site"
  -- Standard SARA entry point
  saraWithClientsAndJobs mClients maybeJobs Nothing defaultSite
