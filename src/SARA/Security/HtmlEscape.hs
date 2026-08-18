module SARA.Security.HtmlEscape
  ( SafeHtml(..)
  , escapeHtml
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

-- | HTML-escape the five reserved characters. '&' is replaced first
--   and only once: every other replacement below inserts a literal '&'
--   as part of its entity (e.g. '"' -> "&quot;"), so escaping '&' after
--   those would re-escape the entities it just introduced, turning
--   "&quot;" into "&amp;quot;" and corrupting the output. Escaping '&'
--   first, before any other entity exists in the string, is what makes
--   this idempotent-on-first-pass and correct for any input containing
--   a mix of the five characters (see SARA.HedgehogSecuritySpec's
--   "HtmlEscape is idempotent" property, and
--   SARA.SecuritySpec's "escapes the five reserved HTML characters" test).
escapeHtml :: Text -> Text
escapeHtml t = T.replace "'" "&#39;"
             . T.replace "\"" "&quot;"
             . T.replace ">" "&gt;"
             . T.replace "<" "&lt;"
             . T.replace "&" "&amp;" $ t

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
