{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module SARA.Template.Renderer
  ( TemplateOracle(..)
  , addTemplateOracle
  , renderTemplate
  ) where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Control.Monad (void)
import qualified Text.Mustache as Mustache
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import SARA.Template.Error
import SARA.Security.HtmlEscape (auditTemplateForRawInterpolation)
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Control.Concurrent (MVar, newMVar, modifyMVar)
import qualified Control.Exception as E

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult TemplateOracle = FilePath

-- | Global cache for compiled templates.
--   Uses a top-level IORef of MVars for double-checked locking per template.
{-# NOINLINE templateCache #-}
templateCache :: IORef (Map.Map FilePath (MVar (Maybe (Either TemplateError Mustache.Template))))
templateCache = unsafePerformIO $ newIORef Map.empty

addTemplateOracle :: Rules ()
addTemplateOracle = void $ addOracle $ \(TemplateOracle path) -> do
  need [path]
  content <- liftIO $ TIO.readFile path
  let auditErrors = auditTemplateForRawInterpolation path content
  case auditErrors of
    [] -> pure path
    errs -> fail $ "Security audit failed for template " ++ path ++ ": " ++ show errs

renderTemplate
  :: FilePath
  -> Aeson.Value
  -> Action (Either TemplateError Text)
renderTemplate tplPath ctx = do
  _ <- askOracle (TemplateOracle tplPath)
  
  -- 1. Get or create the MVar for this specific template path
  mvar <- liftIO $ atomicModifyIORef' templateCache $ \c ->
    case Map.lookup tplPath c of
      Just m  -> (c, m)
      Nothing -> 
        let m = unsafePerformIO (newMVar Nothing)
        in (Map.insert tplPath m c, m)
  
  -- 2. Use the MVar to ensure only one thread compiles this template
  tplRes <- liftIO $ modifyMVar mvar $ \case
    Just res -> return (Just res, res)
    Nothing -> do
      -- This is the first thread to reach here
      res <- E.handle (\(E.SomeException e) -> return $ Left $ TemplateCompileError tplPath (T.pack $ show e)) $
               Right <$> Mustache.compileMustacheFile tplPath
      return (Just res, res)
      
  case tplRes of
    Right tpl -> return $ Right $ TL.toStrict $ Mustache.renderMustache tpl ctx
    Left err  -> return $ Left err
