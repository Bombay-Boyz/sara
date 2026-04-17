{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SARA.Monad
  ( SaraM(..)
  , SaraEnv(..)
  , RuleDecl(..)
  , SiteGraph
  ) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Writer (WriterT, MonadWriter)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.IORef (IORef)
import Data.HashSet (HashSet)
import SARA.Config (SaraConfig, ProjectRoot)
import SARA.Error (AnySaraError)
import SARA.Types (GlobPattern, Item, ValidationState(..), FeedConfig)

-- | The site graph tracks all resolved output paths.
type SiteGraph = HashSet FilePath

-- | The SARA monad stack for rule declaration.
newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv (WriterT [RuleDecl] (ExceptT [AnySaraError] IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SaraEnv, MonadWriter [RuleDecl], MonadError [AnySaraError])

data SaraEnv = SaraEnv
  { envConfig     :: !SaraConfig
  , envRoot       :: !ProjectRoot
  , envSiteGraph  :: !(IORef SiteGraph)
  , envRemapRules :: ![(Text, Text)]
  , envHasErrors  :: !(IORef Bool)
  }

-- | Declarations produced by the DSL.
data RuleDecl
  = RuleMatch    !GlobPattern !(FilePath -> SaraM ())
  | RuleDiscover !GlobPattern
  | RuleRender   !FilePath !(Item 'Validated) !FilePath
  | RuleRenderRaw !Text !(Item 'Validated) !FilePath
  | RuleRemap    ![(Text, Text)]
  | RuleSearch   !FilePath ![Item 'Validated]
  | RulePartialSearch !FilePath !(Item 'Validated)
  | RuleSitemap  !FilePath ![Item 'Validated]
  | RuleRSS      !FilePath !FeedConfig ![Item 'Validated]
  | RuleGlobal   !(SaraM ())
