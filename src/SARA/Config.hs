{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SARA.Config
  ( SaraConfig(..)
  , ProjectRoot
  , mkProjectRoot
  , defaultConfig
  , loadConfig
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON)
import qualified Data.Yaml as Yaml
import SARA.Security.PathGuard (ProjectRoot, mkProjectRoot)
import System.Directory (doesFileExist)

-- | Global configuration for a SARA site.
data SaraConfig = SaraConfig
  { cfgSiteTitle       :: !Text
  , cfgSiteUrl         :: !Text
  , cfgSiteAuthor      :: !Text
  , cfgDefaultTemplate :: !FilePath
  , cfgOutputDirectory :: !FilePath
  , cfgDryRun          :: !Bool
  } deriving (Show, Eq, Generic)

instance FromJSON SaraConfig

defaultConfig :: SaraConfig
defaultConfig = SaraConfig
  { cfgSiteTitle       = "SARA Site"
  , cfgSiteUrl         = "http://localhost:8080"
  , cfgSiteAuthor      = "SARA Developer"
  , cfgDefaultTemplate = "templates/default.html"
  , cfgOutputDirectory = "_site"
  , cfgDryRun          = False
  }

-- | Load configuration from a YAML file, or return defaults.
loadConfig :: FilePath -> IO SaraConfig
loadConfig path = do
  exists <- doesFileExist path
  if exists
    then Yaml.decodeFileEither path >>= \case
      Right cfg -> return cfg
      Left _ -> return defaultConfig
    else return defaultConfig
