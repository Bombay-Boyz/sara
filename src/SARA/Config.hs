{-# LANGUAGE OverloadedStrings #-}

module SARA.Config
  ( SaraConfig(..)
  , ProjectRoot
  , mkProjectRoot
  , defaultConfig
  ) where

import Data.Text (Text)
import SARA.Security.PathGuard (ProjectRoot, mkProjectRoot)

-- | Global configuration for a SARA site.
data SaraConfig = SaraConfig
  { cfgSiteTitle       :: !Text
  , cfgSiteUrl         :: !Text
  , cfgSiteAuthor      :: !Text
  , cfgDefaultTemplate :: !FilePath
  , cfgOutputDirectory :: !FilePath
  , cfgDryRun          :: !Bool
  } deriving (Show, Eq)

defaultConfig :: SaraConfig
defaultConfig = SaraConfig
  { cfgSiteTitle       = "SARA Site"
  , cfgSiteUrl         = "http://localhost:8080"
  , cfgSiteAuthor      = "SARA Developer"
  , cfgDefaultTemplate = "templates/default.html"
  , cfgOutputDirectory = "_site"
  , cfgDryRun          = False
  }
