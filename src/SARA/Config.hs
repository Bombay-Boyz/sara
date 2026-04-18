{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

module SARA.Config
  ( SaraConfig(..)
  , ProjectRoot
  , mkProjectRoot
  , defaultConfig
  , loadConfig
  ) where

import Data.Text (Text)
import qualified Data.Yaml as Yaml
import GHC.Generics (Generic)
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import SARA.Security.PathGuard (ProjectRoot, mkProjectRoot)
import qualified Data.Aeson as Aeson
import System.Exit (exitFailure)
import qualified Data.Text.IO as TIO

data SaraConfig = SaraConfig
  { cfgSiteTitle       :: !Text
  , cfgSiteUrl         :: !Text
  , cfgSiteAuthor      :: !Text
  , cfgOutputDirectory :: !FilePath
  , cfgDryRun          :: !Bool
  } deriving (Show, Generic)

instance Aeson.FromJSON SaraConfig where
  parseJSON = Aeson.withObject "SaraConfig" $ \v -> SaraConfig
    <$> v Aeson..: "title"
    <*> v Aeson..: "baseUrl"
    <*> v Aeson..: "author"
    <*> v Aeson..:? "outputDir" Aeson..!= "_site"
    <*> v Aeson..:? "dryRun" Aeson..!= False

defaultConfig :: SaraConfig
defaultConfig = SaraConfig
  { cfgSiteTitle       = "SARA Site"
  , cfgSiteUrl         = "http://localhost:8000"
  , cfgSiteAuthor      = "SARA Author"
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
      Left err -> do
        putStrLn $ "SARA CONFIG ERROR: Failed to parse " ++ path ++ ": " ++ show err
        exitFailure
    else return defaultConfig
