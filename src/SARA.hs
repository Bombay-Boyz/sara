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
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import Data.IORef (newIORef, readIORef)
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (getCurrentDirectory)
import System.Exit (exitFailure)
import Control.Concurrent (MVar)
import qualified Data.Aeson as Aeson

-- | Entry point for a SARA site.
sara :: SaraM () -> IO ()
sara m = saraWithClients Nothing m

-- | Entry point that supports broadcasting to live clients.
saraWithClients :: Maybe (MVar ClientList) -> SaraM () -> IO ()
saraWithClients mClients m = do
  cwd <- getCurrentDirectory
  root <- mkProjectRoot cwd
  let config = SaraConfig
        { cfgSiteTitle = "SARA Site"
        , cfgSiteUrl = "http://localhost:8080"
        , cfgSiteAuthor = "SARA"
        , cfgDefaultTemplate = "templates/post.html"
        , cfgOutputDirectory = "_site"
        , cfgDryRun = False
        }
  
  graphRef <- newIORef HS.empty
  errorRef <- newIORef False
  
  -- Step 1: Execute DSL to collect RuleDecls
  let initialEnv = SaraEnv
        { envConfig = config
        , envRoot = root
        , envSiteGraph = graphRef
        , envRemapRules = []
        , envHasErrors = errorRef
        }
  
  result <- runExceptT $ runWriterT $ runReaderT (unSaraM m) initialEnv
  
  case result of
    Left errs -> do
      mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
      exitFailure
    Right ((), rules) -> do
      let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
      let finalEnv = initialEnv { envRemapRules = allRemapRules }
      
      runBuild finalEnv rules

      hasErrors <- readIORef (envHasErrors finalEnv)
      siteGraph <- readIORef (envSiteGraph finalEnv)
      
      let itemCount = HS.size siteGraph
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
      
      -- Broadcast Quality Seal to any connected dashboards
      case mClients of
        Just clients -> broadcastMessage clients $ Aeson.object ["type" Aeson..= ("quality-seal" :: T.Text), "data" Aeson..= qs]
        Nothing -> return ()

      if hasErrors 
        then do
          TIO.putStrLn "\nSARA: Build completed with validation errors."
          exitFailure
        else TIO.putStrLn "\nSARA: Build completed successfully."
