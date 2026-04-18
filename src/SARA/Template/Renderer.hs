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
import System.Directory (canonicalizePath)
import qualified Text.Mustache as Mustache
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import SARA.Security.HtmlEscape (auditTemplateForRawInterpolation)
import SARA.Security.PathGuard (guardPath, unSafePath)
import SARA.Monad (SaraEnv(..), SaraState(..))
import SARA.Error (SaraError(..), SaraErrorKind(..), AnySaraError(..), renderAnyErrorColor)
import UnliftIO.IORef
import qualified Data.Map.Strict as Map
import Control.Concurrent.MVar (modifyMVar)
import qualified UnliftIO.Exception as E
import Control.Exception (SomeAsyncException, throwIO)
import qualified Text.Megaparsec as MP

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult TemplateOracle = FilePath

addTemplateOracle :: SaraEnv -> Rules ()
addTemplateOracle env = void $ addOracle $ \(TemplateOracle path) -> do
  need [path]
  res <- liftIO $ guardPath (envRoot env) path
  case res of
    Left err -> fail $ T.unpack (renderAnyErrorColor (AnySaraError err))
    Right safePath -> do
      content <- liftIO $ TIO.readFile (unSafePath safePath)
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
  absTpl <- liftIO $ canonicalizePath tplPathString
  let tplPath = T.pack absTpl
  
  -- 1. Get the pre-initialized MVar for this specific template path (Fix U-02)
  state <- liftIO $ readIORef (envState env)
  case Map.lookup tplPath (stateTemplateCache state) of
    Just mvar -> do
      -- 2. Use the MVar to ensure only one thread compiles this template
      tplRes <- liftIO $ modifyMVar mvar $ \case
        Just res -> return (Just res, res)
        Nothing -> do
          -- This is the first thread to reach here
          res <- (Right <$> Mustache.compileMustacheFile tplPathString) `E.catches` 
                   [ E.Handler $ \(e :: Mustache.MustacheException) -> 
                       let (ln, col, msg) = extractMustacheErrorDetails e
                       in return $ Left $ TemplateCompileError tplPath ln col msg
                   , E.Handler $ \(e :: SomeAsyncException) -> throwIO e
                   , E.Handler $ \(e :: E.SomeException) -> 
                       return $ Left $ TemplateCompileError tplPath Nothing Nothing (T.pack $ show e)
                   ]
          return (Just res, res)
          
      case tplRes of
        Right tpl -> return $ Right $ TL.toStrict $ Mustache.renderMustache tpl ctx
        Left err  -> return $ Left err
    Nothing -> do
      -- Should never happen if runBuild pre-populated correctly
      return $ Left $ TemplateNotFound tplPath

-- | Extract line, column and message from MustacheException if possible.
extractMustacheErrorDetails :: Mustache.MustacheException -> (Maybe Int, Maybe Int, Text)
extractMustacheErrorDetails (Mustache.MustacheParserException bundle) =
    (Nothing, Nothing, T.pack $ MP.errorBundlePretty bundle)
