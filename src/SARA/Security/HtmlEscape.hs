module SARA.Security.HtmlEscape
  ( SafeHtml(..)
  , escapeHtmlValue
  , auditTemplateForRawInterpolation
  , saraManagedPrefix
  ) where

import Data.Aeson (Value(..))
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))

-- | Opaque newtype. A 'SafeHtml' value has been HTML-escaped.
newtype SafeHtml = SafeHtml { unSafeHtml :: Text }
  deriving (Eq, Show)

-- | Recursively HTML-escape all String values in an Aeson Value tree.
escapeHtmlValue :: Value -> Value
escapeHtmlValue = \case
  String t -> String (escapeHtml t)
  Array  a -> Array  (fmap escapeHtmlValue a)
  Object o -> Object (KM.map escapeHtmlValue o)
  other    -> other

-- | Simple HTML escaping for SARA.
escapeHtml :: Text -> Text
escapeHtml t = T.replace "<" "&lt;" 
             . T.replace ">" "&gt;" 
             . T.replace "&" "&amp;" 
             . T.replace "\"" "&quot;" 
             . T.replace "'" "&#39;" $ t

-- | Scan a template file's text for {{{ }}} patterns.
auditTemplateForRawInterpolation
  :: FilePath   -- ^ Template path (for error messages)
  -> Text       -- ^ Template source text
  -> [SaraError 'EKTemplate]
auditTemplateForRawInterpolation path content = 
  -- Search for patterns like {{{ key }}}
  let occurrences = findRawInterpolation content
  in [ TemplateUnsafeInterpolation path line 
     | (key, line) <- occurrences
     , let k = T.strip key
     , not (saraManagedPrefix `T.isPrefixOf` k || k == "itemBody")
     ]

saraManagedPrefix :: Text
saraManagedPrefix = "sara."

-- | Heuristic to find {{{ }}} and their line numbers.
findRawInterpolation :: Text -> [(Text, Int)]
findRawInterpolation content = 
  let lines_ = T.lines content
  in concat [ [ (T.takeWhile (/= '}') (T.drop 3 (snd pair)), line) | pair <- T.breakOnAll "{{{" l ] 
            | (l, line) <- zip lines_ [1..] ]

-- Wait, T.breakOnAll returns a list of (prefix, suffix).
-- So {{{ key }}} would have suffix starting with {{{ key }}}.
-- T.drop 3 rest would be " key }}}...".
-- T.takeWhile (/= '}') would be " key ".
