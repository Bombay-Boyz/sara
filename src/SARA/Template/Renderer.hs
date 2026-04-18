{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

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
import qualified Text.Mustache.Type as Mustache
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import SARA.Security.HtmlEscape (auditTemplateForRawInterpolation)
import SARA.Monad (SaraEnv(..), SaraState(..), SPath)
import SARA.Error (SaraError(..), SaraErrorKind(..))
import UnliftIO.IORef
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (MVar, newMVar, modifyMVar)
import qualified UnliftIO.Exception as E
import qualified Text.Megaparsec as MP

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult TemplateOracle = FilePath

addTemplateOracle :: Rules ()
addTemplateOracle = void $ addOracle $ \(TemplateOracle path) -> do
  need [path]
  content <- liftIO $ TIO.readFile path
  let auditErrors = auditTemplateForRawInterpolation path content
  case auditErrors of
    [] -> pure path
    errs -> fail $ "Security audit failed for template " ++ path ++ ": " ++ show errs

renderTemplate
  :: SaraEnv
  -> FilePath
  -> Aeson.Value
  -> Action (Either (SaraError 'EKTemplate) Text)
renderTemplate env tplPathString ctx = do
  _ <- askOracle (TemplateOracle tplPathString)
  let tplPath = T.pack tplPathString
  
  -- 1. Get or create the MVar for this specific template path
  mvar <- atomicModifyIORef' (envState env) $ \s ->
    case Map.lookup tplPath (stateTemplateCache s) of
      Just m  -> (s, m)
      Nothing -> 
        let m = unsafePerformIO (newMVar Nothing)
        in (s { stateTemplateCache = Map.insert tplPath m (stateTemplateCache s) }, m)
  
  -- 2. Use the MVar to ensure only one thread compiles this template
  tplRes <- liftIO $ modifyMVar mvar $ \case
    Just res -> return (Just res, res)
    Nothing -> do
      -- This is the first thread to reach here
      res <- (Right <$> Mustache.compileMustacheFile tplPathString) `E.catches` 
               [ E.Handler $ \(e :: Mustache.MustacheException) -> 
                   let (ln, col, msg) = extractMustacheErrorDetails e
                   in return $ Left $ TemplateCompileError tplPath ln col msg
               , E.Handler $ \(e :: E.SomeException) -> 
                   return $ Left $ TemplateCompileError tplPath Nothing Nothing (T.pack $ show e)
               ]
      return (Just res, res)
      
  case tplRes of
    Right tpl -> return $ Right $ TL.toStrict $ Mustache.renderMustache tpl ctx
    Left err  -> return $ Left err

-- | Extract line, column and message from MustacheException if possible.
extractMustacheErrorDetails :: Mustache.MustacheException -> (Maybe Int, Maybe Int, Text)
extractMustacheErrorDetails e = case e of
  Mustache.MustacheParserException bundle ->
    (Nothing, Nothing, T.pack $ MP.errorBundlePretty bundle)
  _ -> (Nothing, Nothing, T.pack $ show e)
