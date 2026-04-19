{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.Security.HtmlEscape
  ( escapeHtmlValue
  , auditTemplateForRawInterpolation
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))
import Text.Regex.TDFA ((=~))

-- | Recursively escapes HTML tags in Aeson Values.
escapeHtmlValue :: Aeson.Value -> Aeson.Value
escapeHtmlValue (Aeson.String t) = Aeson.String (escapeHtml t)
escapeHtmlValue (Aeson.Array a)  = Aeson.Array (fmap escapeHtmlValue a)
escapeHtmlValue (Aeson.Object o) = Aeson.Object (KM.map escapeHtmlValue o)
escapeHtmlValue v                = v

-- | Basic HTML entity escaping.
--   Uses a single pass to avoid double escaping of &.
escapeHtml :: Text -> Text
escapeHtml = T.concatMap escapeChar
  where
    escapeChar c = case c of
      '&'  -> "&amp;"
      '<'  -> "&lt;"
      '>'  -> "&gt;"
      '"'  -> "&quot;"
      '\'' -> "&#39;"
      _    -> T.singleton c

-- | Safe keys that are allowed to use raw interpolation.
--   LQIP tokens are managed by SARA and are safe base64 strings.
saraManagedPrefix :: Text
saraManagedPrefix = "SARA_LQIP:"

-- | Scans a template for raw interpolation (three mustaches) 
--   and ensures they only use SARA-managed safe keys.
auditTemplateForRawInterpolation :: FilePath -> Text -> [SaraError 'EKTemplate]
auditTemplateForRawInterpolation pathString content =
  let path = T.pack pathString
      -- Match {{{ key }}} or {{& key }}
      regex = "\\{\\{\\{([^}]+)\\}\\}\\}|\\{\\{\\&([^}]+)\\}\\}" :: Text
      
      lines' = T.lines content
      occurrences = [ (key, ln) 
                    | (ln, line) <- zip [1..] lines'
                    , let matches = (line =~ regex :: [[Text]])
                    , m <- matches
                    , let key = extractKey m
                    , not (T.null key)
                    ]

  in [ TemplateUnsafeInterpolation path line 
     | (key, line) <- occurrences
     , let k = T.strip key
     , not (saraManagedPrefix `T.isPrefixOf` k || k == "itemBody")]

-- | Safely extracts the first non-empty capture group from a match.
extractKey :: [Text] -> Text
extractKey (_:g1:g2:_) = if not (T.null g1) then g1 else g2
extractKey _           = ""
