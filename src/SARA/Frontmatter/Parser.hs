module SARA.Frontmatter.Parser
  ( parseFrontmatter
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Yaml as Yaml
import qualified Toml as Toml
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V
import SARA.Frontmatter.Detect (FrontmatterFormat(..), detectFormat, splitFrontmatter)
import SARA.Error (SaraError(..), SaraErrorKind(..), SourcePos(..))

-- | UNIVERSAL PARSER: parse → Aeson Value. Values are returned exactly
--   as authored — this module does not HTML-escape them.
--
--   That is a deliberate change from an earlier version, which escaped
--   every string value here on the theory that "escape once, at the
--   earliest point untrusted content enters the pipeline" is safest.
--   In practice that made every *consumer* of 'itemMeta' double-escape:
--   'SARA.Template.Renderer' interpolates metadata through
--   'Text.Microstache's @{{ }}@ variables, which already HTML-escapes
--   (see @Text.Microstache.Render.escapeHtml@) — so a value escaped
--   here arrived pre-mangled (@\"&quot;@ became @\"&amp;quot;@).
--   'SARA.SEO.Feed' embeds the same values as 'Text.XML' element
--   content, which XML-escapes on serialisation for the same reason —
--   another silent double-escape. 'SARA.SEO.JsonLD' embeds them as
--   plain JSON string values, where HTML entities were simply wrong
--   output, not a security measure at all.
--
--   Escaping is therefore the responsibility of whichever consumer
--   emits a format that needs it, applied exactly once, at the point
--   the value is written into that format — which for the Mustache
--   path is already handled by the template engine itself, matching
--   1.5\/2.2 of the Haskell Engineering Standard (validation and
--   escaping are staged per pipeline stage, not scattered, and
--   certainly not applied twice by two different stages that don't
--   know about each other).
parseFrontmatter
  :: FilePath         -- ^ For error messages
  -> Text             -- ^ Full file content
  -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseFrontmatter path content = do
  let fmt = detectFormat strippedContent
  case fmt of
    FmNone -> Right (KM.empty, strippedContent)
    _ -> do
      (rawFM, body) <- splitFrontmatter fmt strippedContent
      case fmt of
        FmYAML -> parseYAML path rawFM body
        FmTOML -> parseTOML path rawFM body
        FmJSON -> parseJSON path rawFM body
  where
    -- A leading UTF-8 byte-order-mark (U+FEFF) is legal, invisible in
    -- most editors, and produced by some Windows tools (Notepad among
    -- them) by default. Without stripping it, 'detectFormat's
    -- "---"/"+++"/"{" prefix check silently fails — the file has real
    -- frontmatter, but SARA would treat it as if it had none, with no
    -- error at all (Jekyll's own docs describe exactly this failure
    -- mode: a stray BOM causes "very, very bad things" during parsing,
    -- precisely because it's invisible and the failure is silent, not
    -- a loud rejection). Stripped once, here, at the single entry
    -- point every content file passes through — not duplicated at
    -- 'detectFormat' and 'splitFrontmatter' separately.
    strippedContent = case T.uncons content of
      Just ('\xFEFF', rest) -> rest
      _                     -> content

parseYAML :: FilePath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseYAML path raw body = 
  case Yaml.decodeEither' (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack $ show err)
    Right val -> case val of
      Aeson.Object obj -> Right (obj, body)
      _ -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) "Expected Object"

parseTOML :: FilePath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseTOML path raw body = 
  case Toml.parse raw of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right table -> 
      let obj = tableToAeson (Toml.forgetTableAnns table)
      in Right (obj, body)

parseJSON :: FilePath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseJSON path raw body = 
  case Aeson.eitherDecodeStrict (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right obj -> Right (obj, body)

tableToAeson :: Toml.Table -> Aeson.Object
tableToAeson (Toml.MkTable m) = KM.fromList 
  [ (K.fromText k, valueToAeson v) 
  | (k, ((), v)) <- Map.toList m 
  ]

valueToAeson :: Toml.Value -> Aeson.Value
valueToAeson v = case v of
  Toml.Bool b -> Aeson.Bool b
  Toml.Integer i -> Aeson.Number (fromIntegral i)
  Toml.Double d -> Aeson.Number (realToFrac d)
  Toml.Text t -> Aeson.String t
  Toml.ZonedTime t -> Aeson.String (T.pack $ show t)
  Toml.LocalTime t -> Aeson.String (T.pack $ show t)
  Toml.Day t -> Aeson.String (T.pack $ show t)
  Toml.TimeOfDay t -> Aeson.String (T.pack $ show t)
  Toml.List a -> Aeson.Array (V.fromList $ fmap valueToAeson a)
  Toml.Table t -> Aeson.Object (tableToAeson t)
