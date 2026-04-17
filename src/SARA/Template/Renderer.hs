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

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult TemplateOracle = FilePath

-- | Global cache for compiled templates.
--   Justified because it's a pure cache of immutable-once-loaded data.
{-# NOINLINE templateCache #-}
templateCache :: IORef (Map.Map FilePath Mustache.Template)
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
  
  -- Check cache first
  cache <- liftIO $ readIORef templateCache
  tpl <- case Map.lookup tplPath cache of
    Just t -> return t
    Nothing -> do
      -- Double-checked pattern: multiple threads might reach here, 
      -- but compileMustacheFile is idempotent for our purposes.
      -- To be truly robust, we could use an MVar per template, but that's overkill.
      -- We'll just ensure we don't overwrite a newer entry if someone else finished first.
      res <- liftIO $ Mustache.compileMustacheFile tplPath
      liftIO $ atomicModifyIORef' templateCache $ \c -> 
        case Map.lookup tplPath c of
          Just existing -> (c, existing)
          Nothing -> (Map.insert tplPath res c, res)
      
  let result = TL.toStrict $ Mustache.renderMustache tpl ctx
  pure $ Right result
