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
import Control.Monad.Reader (runReaderT)
import Data.IORef (newIORef, readIORef)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import Control.Concurrent (MVar)
import qualified Data.Aeson as Aeson
import System.CPUTime (getCPUTime)
import Control.Exception (try, SomeException)

-- | Entry point for a SARA site.
sara :: SaraM () -> IO ()
sara m = saraWithClients Nothing m

-- | Entry point that supports broadcasting to live clients.
saraWithClients :: Maybe (MVar ClientList) -> SaraM () -> IO ()
saraWithClients mClients m = do
  start <- getCPUTime
  cwd <- getCurrentDirectory
  root <- mkProjectRoot cwd
  
  -- Step 0: Load config
  config <- loadConfig (cwd </> "sara.yaml")
  
  stateRef <- newIORef initialState
  
  -- Step 1: Execute DSL to collect RuleDecls
  let initialEnv = SaraEnv
        { envConfig     = config
        , envRoot       = root
        , envIsPlanning = True
        , envRemapRules = []
        , envState      = stateRef
        }
  
  -- Refactored to catch exceptions in IO
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
      let picosPerSec = 10^(12 :: Integer)
      let diffSecs = fromIntegral (end - start) / fromIntegral picosPerSec
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
