{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SARA.Monad
  ( SaraM(..)
  , SaraEnv(..)
  , RuleDecl(..)
  , SiteGraph
  , tellRule
  ) where

import Control.Monad.Reader (ReaderT, MonadReader, asks)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import Data.IORef (IORef, atomicModifyIORef')
import Data.HashSet (HashSet)
import qualified Data.Map.Strict as Map
import SARA.Config (SaraConfig, ProjectRoot)
import SARA.Error (AnySaraError)
import SARA.Types (GlobPattern, Item, ValidationState(..), FeedConfig)

-- | The site graph tracks all resolved output paths.
type SiteGraph = HashSet FilePath

-- | The SARA monad stack for rule declaration.
newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv (ExceptT [AnySaraError] IO) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SaraEnv, MonadError [AnySaraError])

-- | Register a rule in the SARA environment.
tellRule :: RuleDecl -> SaraM ()
tellRule d = do
  ref <- asks envRules
  liftIO $ atomicModifyIORef' ref (\rules -> (d : rules, ()))

data SaraEnv = SaraEnv
  { envConfig     :: !SaraConfig
  , envRoot       :: !ProjectRoot
  , envSiteGraph  :: !(IORef SiteGraph)
  , envRemapRules :: ![(Text, Text)]
  , envHasErrors  :: !(IORef Bool)
  , envRules      :: !(IORef [RuleDecl])
  , envIsPlanning :: !Bool
  , envItemCache  :: !(IORef (Map.Map FilePath (Item 'Validated)))
  , envDataCache  :: !(IORef (Map.Map FilePath Aeson.Value))
  , envCurrentDeps :: !(IORef [FilePath])
  }


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
