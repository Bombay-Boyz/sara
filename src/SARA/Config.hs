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
import SARA.Security.PathGuard (ProjectRoot, mkProjectRoot)
import qualified Data.Aeson as Aeson
import System.Exit (exitFailure)
import Data.List (isPrefixOf)

data SaraConfig = SaraConfig
  { cfgSiteTitle       :: !Text
  , cfgSiteUrl         :: !Text
  , cfgSiteAuthor      :: !Text
  , cfgOutputDirectory :: !FilePath
  , cfgDryRun          :: !Bool
  , cfgAllowedCommands :: ![Text]
  , cfgDefaultImageQuality :: !Int
  , cfgDefaultImageFormats :: ![Text] -- e.g. ["webp", "avif"]
  , cfgSEOReportPath      :: !(Maybe FilePath)
  } deriving (Show, Generic)

instance Aeson.FromJSON SaraConfig where
  parseJSON = Aeson.withObject "SaraConfig" $ \v -> SaraConfig
    <$> v Aeson..: "title"
    <*> v Aeson..: "baseUrl"
    <*> v Aeson..: "author"
    <*> v Aeson..:? "outputDir" Aeson..!= "_site"
    <*> v Aeson..:? "dryRun" Aeson..!= False
    <*> v Aeson..:? "allowedCommands" Aeson..!= ["cp", "mv", "mkdir", "convert", "ffmpeg"]
    <*> v Aeson..:? "imageQuality" Aeson..!= 80
    <*> v Aeson..:? "imageFormats" Aeson..!= ["webp", "jpg"]
    <*> v Aeson..:? "seoReportPath" Aeson..!= Just ".sara/seo-report.json"

defaultConfig :: SaraConfig
defaultConfig = SaraConfig
  { cfgSiteTitle       = "SARA Site"
  , cfgSiteUrl         = "http://localhost:8000"
  , cfgSiteAuthor      = "SARA Author"
  , cfgOutputDirectory = "_site"
  , cfgDryRun          = False
  , cfgAllowedCommands = ["cp", "mv", "mkdir", "convert", "ffmpeg"]
  , cfgDefaultImageQuality = 80
  , cfgDefaultImageFormats = ["webp", "jpg"]
  , cfgSEOReportPath      = Just ".sara/seo-report.json"
  }

-- | Load configuration from a YAML file, or return defaults.
loadConfig :: FilePath -> IO SaraConfig
loadConfig path = do
  exists <- doesFileExist path
  cfg <- if exists
    then Yaml.decodeFileEither path >>= \case
      Right c -> return c
      Left err -> do
        putStrLn $ "SARA CONFIG ERROR: Failed to parse " ++ path ++ ": " ++ show err
        exitFailure
    else return defaultConfig
  
  -- Industrial Grade: Validate output directory
  let outDir = cfgOutputDirectory cfg
  if ".." `isPrefixOf` outDir || "/" `isPrefixOf` outDir || null outDir
    then do
      putStrLn $ "SARA CONFIG ERROR: outputDir '" ++ outDir ++ "' is unsafe or invalid. Must be a relative path within project."
      exitFailure
    else return cfg
