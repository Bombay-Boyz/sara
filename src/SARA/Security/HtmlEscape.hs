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
saraManagedPrefix = "__LQIP__:"

-- | Scans a template for raw interpolation (three mustaches) 
--   and ensures they only use SARA-managed safe keys.
auditTemplateForRawInterpolation :: FilePath -> Text -> [SaraError 'EKTemplate]
auditTemplateForRawInterpolation pathString content =
  let path = T.pack pathString
      -- Match {{{ key }}} or {{& key }}
      regex = "\\{\\{\\{([^}]+)\\}\\}\\}|\\{\\{\\&([^}]+)\\}\\}" :: Text
      
      -- Extract matches with line numbers (simplified)
      -- In a real version, we'd use a parser that tracks SourcePos
      lines' = T.lines content
      occurrences = [ (key, ln) 
                    | (ln, line) <- zip [1..] lines'
                    , let matches = (line =~ regex :: [[Text]])
                    , not (null matches)
                    , m <- matches
                    , let key = if not (T.null (m !! 1)) then m !! 1 else m !! 2
                    ]

  in [ TemplateUnsafeInterpolation path line 
     | (key, line) <- occurrences
     , let k = T.strip key
     , not (saraManagedPrefix `T.isPrefixOf` k || k == "itemBody")]
