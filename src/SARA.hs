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
import SARA.Monad (SaraM(..), SaraEnv(..), RuleDecl(..))
import SARA.Security.ShellGuard (validateArg)
import SARA.Internal.Engine (runBuild)
import SARA.Diagnostics (QualitySeal(..), renderQualitySeal)
import SARA.LiveReload.Server (broadcastMessage, ClientList)
import SARA.Template.Lucid (renderLucid)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Data.IORef (newIORef, readIORef)
import qualified Data.Map.Strict as Map
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.FilePath ((</>))
import System.Exit (exitFailure)
import Control.Concurrent (MVar)
import qualified Data.Aeson as Aeson
import System.CPUTime (getCPUTime)

-- | Entry point for a SARA site.
sara :: SaraM () -> IO ()
sara m = saraWithClients Nothing m

-- | Entry point that supports broadcasting to live clients.
saraWithClients :: Maybe (MVar ClientList) -> SaraM () -> IO ()
saraWithClients mClients m = do
  start <- getCPUTime
  cwd <- getCurrentDirectory
  root <- mkProjectRoot cwd
  
  -- Step 0: Load config (Fixes Issue #19)
  config <- loadConfig (cwd </> "sara.yaml")
  
  graphRef <- newIORef HS.empty
  errorRef <- newIORef False
  rulesRef <- newIORef []
  itemCacheRef <- newIORef Map.empty
  dataCacheRef <- newIORef Map.empty
  
  -- Step 1: Execute DSL to collect RuleDecls
  let initialEnv = SaraEnv
        { envConfig = config
        , envRoot = root
        , envSiteGraph = graphRef
        , envRemapRules = []
        , envHasErrors = errorRef
        , envRules = rulesRef
        , envIsPlanning = True
        , envItemCache = itemCacheRef
        , envDataCache = dataCacheRef
        }
  
  result <- runExceptT $ runReaderT (unSaraM m) initialEnv
  
  case result of
    Left errs -> do
      mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
      exitFailure
    Right () -> do
      rules <- readIORef rulesRef
      
      let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
      let finalEnv = initialEnv { envRemapRules = allRemapRules, envIsPlanning = False }
      
      runBuild finalEnv rules

      hasErrors <- readIORef (envHasErrors finalEnv)
      siteGraph <- readIORef (envSiteGraph finalEnv)
      
      end <- getCPUTime
      let itemCount = HS.size siteGraph
      -- A simplified industrial performance score: 
      -- Based on items per second to give some relation to speed.
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
      
      -- Broadcast Quality Seal to any connected dashboards
      case mClients of
        Just clients -> broadcastMessage clients $ Aeson.object ["type" Aeson..= ("quality-seal" :: T.Text), "data" Aeson..= qs]
        Nothing -> return ()

      if hasErrors 
        then do
          TIO.putStrLn "\nSARA: Build completed with validation errors."
          exitFailure
        else TIO.putStrLn "\nSARA: Build completed successfully."
