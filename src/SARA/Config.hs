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
  -- | Whether markdown content may embed raw HTML (script tags,
  --   inline event handlers, iframes — anything CommonMark's default
  --   HTML-escaping would otherwise neutralise). Defaults to 'False':
  --   enabling this means the site owner is vouching for *every*
  --   markdown file's HTML content, including migrated posts, guest
  --   contributions, and anything else that isn't 100% first-party —
  --   see 'SARA.Markdown.Parser.parseMarkdown' and audit issue #1.
  , cfgAllowRawHtml    :: !Bool
  } deriving (Show, Eq)

defaultConfig :: SaraConfig
defaultConfig = SaraConfig
  { cfgSiteTitle       = "SARA Site"
  , cfgSiteUrl         = "http://localhost:8080"
  , cfgSiteAuthor      = "SARA Developer"
  , cfgDefaultTemplate = "templates/default.html"
  , cfgOutputDirectory = "_site"
  , cfgDryRun          = False
  , cfgAllowRawHtml    = False
  }
