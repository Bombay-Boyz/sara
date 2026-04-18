{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}

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
  ) where

import Control.Monad.Reader (ReaderT, MonadReader, ask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import Data.IORef (IORef, atomicModifyIORef')
import Data.HashSet (HashSet)
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import SARA.Config (SaraConfig, ProjectRoot)
import SARA.Error (AnySaraError, SaraError, SaraErrorKind(..))
import SARA.Types (GlobPattern, Item, ValidationState(..), FeedConfig)
import qualified Text.Mustache as Mustache
import Control.Concurrent (MVar)
import GHC.Generics (Generic)
import Control.Monad (unless)
import SARA.Markdown.Shortcode (Shortcode)

-- | The site graph tracks all resolved output paths.
type SiteGraph = HashSet FilePath

-- | Pure state of the SARA application.
--   Consolidating the "IORef Soup" into a single state record.
data SaraState = SaraState
  { stateSiteGraph     :: !SiteGraph
  , stateHasErrors     :: !Bool
  , stateRules         :: ![RuleDecl]
  , stateLocalRules    :: ![RuleDecl]
  , stateItemCache     :: !(Map.Map FilePath (Item 'Validated))
  , stateDataCache     :: !(Map.Map FilePath Aeson.Value)
  , stateTemplateCache :: !(Map.Map FilePath (MVar (Maybe (Either (SaraError 'EKTemplate) Mustache.Template))))
  , stateCurrentDeps   :: ![FilePath]
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
--   Refactored to remove ExceptT in favor of IO exceptions (industrial standard).
newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv IO a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SaraEnv)

-- | Register a rule in the SARA environment.
--   Only effective during the planning phase.
tellRule :: RuleDecl -> SaraM ()
tellRule d = do
  env <- ask
  if envIsPlanning env
    then liftIO $ atomicModifyIORef' (envState env) $ \s ->
      (s { stateLocalRules = d : stateLocalRules s }, ())
    else return ()

-- | Commit locally collected rules to the global rule set.
commitRules :: SaraM ()
commitRules = do
  env <- ask
  liftIO $ do
    locals <- atomicModifyIORef' (envState env) $ \s ->
      (s { stateLocalRules = [] }, stateLocalRules s)
    unless (null locals) $
      atomicModifyIORef' (envState env) $ \s ->
        (s { stateRules = locals ++ stateRules s }, ())

-- | Record a dynamic dependency for the current item.
addItemDependency :: FilePath -> SaraM ()
addItemDependency p = do
  env <- ask
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateCurrentDeps = p : stateCurrentDeps s }, ())

-- | Read a file and automatically track it as a dependency.
readFileTracked :: FilePath -> SaraM BS.ByteString
readFileTracked p = do
  addItemDependency p
  liftIO $ BS.readFile p

-- | Read a UTF-8 text file and automatically track it as a dependency.
readTextFileTracked :: FilePath -> SaraM Text
readTextFileTracked p = do
  addItemDependency p
  liftIO $ T.decodeUtf8 <$> BS.readFile p

-- | Declarations produced by the DSL.
data RuleDecl
  = RuleMatch    !GlobPattern !(FilePath -> SaraM (Item 'Validated))
  | RuleDiscover !GlobPattern
  | RuleRender   !FilePath !(Item 'Validated) !FilePath
  | RuleRenderRaw !Text !(Item 'Validated) !FilePath
  | RuleRemap    ![(Text, Text)]
  | RuleSearch   !FilePath ![Item 'Validated]
  | RulePartialSearch !FilePath !(Item 'Validated)
  | RuleSitemap  !FilePath ![Item 'Validated]
  | RuleRSS      !FilePath !FeedConfig ![Item 'Validated]
  | RuleDataDependency !FilePath
  | RuleGlobal   !(SaraM ())
