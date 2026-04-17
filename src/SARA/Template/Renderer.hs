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

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

-- | We must store a serializable version in the Oracle result.
--   Mustache.Template is not easily serializable/NFData.
--   So we store the FilePath and depend on it.
type instance RuleResult TemplateOracle = FilePath

addTemplateOracle :: Rules ()
addTemplateOracle = void $ addOracle $ \(TemplateOracle path) -> do
  -- 1. Read the template
  content <- liftIO $ TIO.readFile path
  
  -- 2. Run security audit
  let auditErrors = auditTemplateForRawInterpolation path content
  case auditErrors of
    [] -> do
      -- 3. Just return the path if audit passed. 
      -- We depend on the template file.
      need [path]
      pure path
    errs -> do
      error $ "Security audit failed for template " ++ path ++ ": " ++ show errs

renderTemplate
  :: FilePath
  -> Aeson.Value
  -> Action (Either TemplateError Text)
renderTemplate tplPath ctx = do
  -- This will trigger the oracle (audit)
  _ <- askOracle (TemplateOracle tplPath)
  -- If we are here, audit passed.
  -- stache has its own cache if we use the same tpl object, 
  -- but here we are in a distributed/parallel build, 
  -- so we compile in each rule (or we could use a better oracle).
  -- For now, compile and render.
  res <- liftIO $ Mustache.compileMustacheFile tplPath
  let result = TL.toStrict $ Mustache.renderMustache res ctx
  pure $ Right result
