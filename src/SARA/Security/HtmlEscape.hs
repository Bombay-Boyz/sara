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
import Data.List (foldl')

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
--   Note: We escape '&' FIRST to avoid double-escaping entities like '&lt;'.
escapeHtml :: Text -> Text
escapeHtml t = foldl' (\acc (old, new) -> T.replace old new acc) t
  [ ("&", "&amp;")
  , ("<", "&lt;")
  , (">", "&gt;")
  , ("\"", "&quot;")
  , ("'", "&#39;")
  ]

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
