module SARA.Frontmatter.Parser
  ( parseFrontmatter
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Yaml as Yaml
import SARA.Internal.Toml (parseTomlToAeson)
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
parseYAML path raw body = do
  checkYamlComplexity path raw
  case Yaml.decodeEither' (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack $ show err)
    Right val -> case val of
      Aeson.Object obj -> Right (obj, body)
      _ -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) "Expected Object"

-- | A coarse, pre-parse guard against YAML's "billion laughs"-style
--   anchor/alias amplification (audit issue #8): libyaml, and the
--   'Data.Yaml' binding over it this module uses, expand anchors and
--   aliases with no built-in cap on the resulting size, unlike every
--   other adversarial-input class this codebase treats explicitly
--   (NUL bytes in 'SARA.Security.PathGuard'\/'SARA.Security.ShellGuard',
--   ReDoS in 'SARA.Security.RegexGuard'). Frontmatter is metadata, not
--   a document body — legitimate frontmatter is small and has no
--   business defining dozens of anchors — so both checks are
--   deliberately conservative: a real frontmatter block will never
--   come close to either threshold, while a crafted one that would
--   force pathological expansion is rejected before libyaml ever
--   sees it.
--
--   This is necessarily a heuristic, not a real YAML parse (a real
--   parse is exactly the expensive operation being guarded against):
--   it counts anchor (@&name@) and alias (@*name@) *markers* by
--   textual scan, which can overcount (e.g. a literal @&@ inside a
--   quoted string) but never undercounts the actual anchor/alias
--   count a real parse would see, so it fails safe.
checkYamlComplexity :: FilePath -> Text -> Either (SaraError 'EKFrontmatter) ()
checkYamlComplexity path raw
  | T.length raw > maxFrontmatterChars =
      Left $ FrontmatterParseFailure path (SourcePos path 1 1) $
        "frontmatter exceeds " <> T.pack (show maxFrontmatterChars)
          <> " characters (rejected before YAML parsing to bound anchor/alias expansion cost)"
  | markerCount > maxAnchorAliasMarkers =
      Left $ FrontmatterParseFailure path (SourcePos path 1 1) $
        "frontmatter contains " <> T.pack (show markerCount)
          <> " YAML anchor/alias markers, exceeding the limit of "
          <> T.pack (show maxAnchorAliasMarkers)
          <> " (rejected to bound anchor/alias expansion cost)"
  | otherwise = Right ()
  where
    maxFrontmatterChars :: Int
    maxFrontmatterChars = 65536

    maxAnchorAliasMarkers :: Int
    maxAnchorAliasMarkers = 64

    markerCount = T.count "&" raw + T.count "*" raw

parseTOML :: FilePath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseTOML path raw body =
  case parseTomlToAeson raw of
    Left err  -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right obj -> Right (obj, body)

parseJSON :: FilePath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseJSON path raw body = 
  case Aeson.eitherDecodeStrict (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right obj -> Right (obj, body)
