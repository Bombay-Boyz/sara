{-# LANGUAGE OverloadedStrings #-}

module SARA
  ( module SARA.DSL
  , module SARA.Types
  , module SARA.Config
  , module SARA.Error
  , module SARA.Template.Lucid
  , SaraM
  , sara
  , saraWithClients
  , saraWithClientsAndJobs
  , validateArg
  ) where

import SARA.DSL
import SARA.Types
import SARA.Config
import SARA.Error
import SARA.Monad (SaraM(..), SaraEnv(..), SaraState(..), RuleDecl(..), initialState)
import SARA.Security.ShellGuard (validateArg)
import SARA.Internal.Engine (runBuild)
import SARA.Diagnostics (QualitySeal(..), renderQualitySeal)
import SARA.LiveReload.Server (broadcastMessage, ClientList)
import SARA.Template.Lucid (renderLucid)
import SARA.Asset.BinaryCheck (verifyBinaries)
import Control.Monad.Reader (runReaderT)
import UnliftIO.IORef (newIORef, readIORef)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import UnliftIO.MVar (MVar)
import qualified Data.Aeson as Aeson
import System.CPUTime (getCPUTime)
import UnliftIO.Exception (try, SomeException)

-- | Entry point for a SARA site.
sara :: SaraM () -> IO ()
sara m = saraWithClientsAndJobs Nothing Nothing Nothing m

-- | Entry point that supports broadcasting to live clients.
saraWithClients :: Maybe (MVar ClientList) -> SaraM () -> IO ()
saraWithClients mClients m = saraWithClientsAndJobs mClients Nothing Nothing m

-- | Entry point that supports broadcasting and parallel job configuration.
saraWithClientsAndJobs :: Maybe (MVar ClientList) -> Maybe Int -> Maybe SaraConfig -> SaraM () -> IO ()
saraWithClientsAndJobs mClients maybeJobs maybeConfig m = do
  start <- getCPUTime
  
  -- Industrial Grade Step 1: Verify Environment Readiness
  binCheck <- verifyBinaries ["cwebp", "convert"] 
  case binCheck of
    Left err -> do
      TIO.putStrLn $ "WARNING: " <> renderAnyErrorColor err
      TIO.putStrLn "SARA will continue, but image processing may fail."
    Right () -> return ()

  cwd <- getCurrentDirectory
  root <- mkProjectRoot cwd
  setCurrentDirectory cwd
  
  -- Step 0: Load config
  config <- case maybeConfig of
    Just c -> return c
    Nothing -> loadConfig (cwd </> "sara.yaml")
  
  stateRef <- newIORef initialState
  
  -- Step 1: Execute DSL to collect RuleDecls
  let initialEnv = SaraEnv
        { envConfig     = config
        , envRoot       = root
        , envIsPlanning = True
        , envRemapRules = []
        , envState      = stateRef
        , envJobs       = maybeJobs
        }
  
  -- Refactored to catch exceptions using UnliftIO
  result <- try (runReaderT (unSaraM m) initialEnv) :: IO (Either SomeException ())
  
  case result of
    Left err -> do
      TIO.putStrLn $ "SARA ERROR: " <> T.pack (show err)
      exitFailure
    Right () -> do
      state <- readIORef stateRef
      let rules = reverse (stateRules state)
      
      let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
      let finalEnv = initialEnv { envRemapRules = allRemapRules, envIsPlanning = False }
      
      -- Industrial refinement: runBuild should also handle its own exceptions or pass them up
      runBuild finalEnv rules

      finalState <- readIORef stateRef
      let hasErrors = stateHasErrors finalState
      let siteGraph = stateSiteGraph finalState
      
      end <- getCPUTime
      let itemCount = HS.size siteGraph
      let picosPerSec :: Integer
          picosPerSec = 10^(12 :: Integer)
      let diffSecs :: Double
          diffSecs = fromIntegral (end - start) / fromIntegral picosPerSec
      let perfScore = if itemCount > 0 && diffSecs > 0
                      then min 100 (floor (fromIntegral itemCount / diffSecs / 10.0))
                      else 0
      
      let qs = QualitySeal
            { qsSecurity = not hasErrors 
            , qsSEO = not hasErrors
            , qsPerformance = perfScore
            , qsItemCount = itemCount
            }
      
      renderQualitySeal qs
      
      case mClients of
        Just clients -> broadcastMessage clients $ Aeson.object ["type" Aeson..= ("quality-seal" :: T.Text), "data" Aeson..= qs]
        Nothing -> return ()

      if hasErrors 
        then do
          TIO.putStrLn "\nSARA: Build completed with validation errors."
          exitFailure
        else TIO.putStrLn "\nSARA: Build completed successfully."
