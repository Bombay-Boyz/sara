{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}

module SARA.Monad
  ( SaraM(..)
  , SaraEnv(..)
  , SaraState(..)
  , RuleDecl(..)
  , SiteGraph
  , tellRule
  , commitRules
  , addItemDependency
  , readFileTracked
  , readTextFileTracked
  , initialState
  , SPath
  ) where

import Control.Monad.Reader (ReaderT, MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import UnliftIO.IORef (IORef, atomicModifyIORef')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import SARA.Config (SaraConfig, ProjectRoot)
import SARA.Error (AnySaraError(..), SaraError(..), SaraErrorKind(..), SourcePos(..))
import SARA.Types (GlobPattern, Item, ValidationState(..), FeedConfig, SPath, Route(..))
import qualified Text.Mustache as Mustache
import UnliftIO.MVar (MVar)
import GHC.Generics (Generic)
import Control.Monad (unless)
import SARA.Markdown.Shortcode (Shortcode)
import UnliftIO.Exception (throwIO)

-- | The site graph tracks all resolved output paths.
type SiteGraph = HashSet SPath

-- | Pure state of the SARA application.
--   Consolidating the "IORef Soup" into a single state record.
data SaraState = SaraState
  { stateSiteGraph     :: !SiteGraph
  , stateHasErrors     :: !Bool
  , stateRules         :: ![RuleDecl]
  , stateLocalRules    :: ![RuleDecl]
  , stateItemCache     :: !(Map.Map SPath (Item 'Validated))
  , stateDataCache     :: !(Map.Map SPath Aeson.Value)
  , stateTemplateCache :: !(Map.Map SPath (MVar (Maybe (Either (SaraError 'EKTemplate) Mustache.Template))))
  , stateCurrentDeps   :: ![SPath]
  , stateShortcodeHandlers :: !(Map.Map Text (Shortcode -> SaraM Text))
  } deriving (Generic)

initialState :: SaraState
initialState = SaraState
  { stateSiteGraph = HS.empty
  , stateHasErrors = False
  , stateRules = []
  , stateLocalRules = []
  , stateItemCache = Map.empty
  , stateDataCache = Map.empty
  , stateTemplateCache = Map.empty
  , stateCurrentDeps = []
  , stateShortcodeHandlers = Map.empty
  }

-- | Read-only environment for SARA.
data SaraEnv = SaraEnv
  { envConfig     :: !SaraConfig
  , envRoot       :: !ProjectRoot
  , envIsPlanning :: !Bool
  , envRemapRules :: ![(Text, Text)]
  , envState      :: !(IORef SaraState)
  }

-- | The SARA monad stack for rule declaration.
newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadUnliftIO, MonadReader SaraEnv)

-- | Register a rule in the SARA environment.
tellRule :: RuleDecl -> SaraM ()
tellRule d = do
  env <- ask
  if envIsPlanning env
    then atomicModifyIORef' (envState env) $ \s ->
      (s { stateLocalRules = d : stateLocalRules s }, ())
    else return ()

-- | Commit locally collected rules to the global rule set.
commitRules :: SaraM ()
commitRules = do
  env <- ask
  locals <- atomicModifyIORef' (envState env) $ \s ->
    (s { stateLocalRules = [] }, stateLocalRules s)
  unless (null locals) $
    atomicModifyIORef' (envState env) $ \s ->
      (s { stateRules = locals ++ stateRules s }, ())

-- | Record a dynamic dependency for the current item.
addItemDependency :: SPath -> SaraM ()
addItemDependency p = do
  env <- ask
  atomicModifyIORef' (envState env) $ \s ->
    (s { stateCurrentDeps = p : stateCurrentDeps s }, ())

-- | Read a file and automatically track it as a dependency.
readFileTracked :: SPath -> SaraM BS.ByteString
readFileTracked p = do
  addItemDependency p
  liftIO $ BS.readFile (T.unpack p)

-- | Read a UTF-8 text file and automatically track it as a dependency.
readTextFileTracked :: SPath -> SaraM Text
readTextFileTracked p = do
  addItemDependency p
  bs <- liftIO $ BS.readFile (T.unpack p)
  case T.decodeUtf8' bs of
    Right t -> return t
    Left err -> do
      throwIO (AnySaraError $ FrontmatterParseFailure p (SourcePos p 1 1) (T.pack $ show err))

-- | Declarations produced by the DSL.
data RuleDecl
  = RuleMatch    !GlobPattern !(SPath -> SaraM (Item 'Validated))
  | RuleDiscover !GlobPattern
  | RuleRender   !SPath !(Item 'Validated) !SPath
  | RuleRenderRaw !Text !(Item 'Validated) !SPath
  | RuleRemap    ![(Text, Text)]
  | RuleSearch   !SPath ![Item 'Validated]
  | RulePartialSearch !SPath !(Item 'Validated)
  | RuleSitemap  !SPath ![Item 'Validated]
  | RuleRSS      !SPath !FeedConfig ![Item 'Validated]
  | RuleDataDependency !SPath
  | RuleGlobal   !(SaraM ())
