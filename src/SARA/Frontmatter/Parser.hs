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
import SARA.Security.HtmlEscape (escapeHtmlValue)
import SARA.Error (SaraError(..), SaraErrorKind(..), SourcePos(..))

import SARA.Monad (SPath)

-- | UNIVERSAL PARSER: parse → Aeson Value → HTML-escape.
parseFrontmatter
  :: SPath            -- ^ For error messages
  -> Text             -- ^ Full file content
  -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseFrontmatter pathText content = do
  let fmt = detectFormat content
  case fmt of
    FmNone -> Right (KM.empty, content)
    _ -> do
      (rawFM, body) <- splitFrontmatter fmt content
      case fmt of
        FmYAML -> parseYAML pathText rawFM body
        FmTOML -> parseTOML pathText rawFM body
        FmJSON -> parseJSON pathText rawFM body

parseYAML :: SPath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseYAML path raw body = 
  case Yaml.decodeEither' (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack $ show err)
    Right val -> case val of
      Aeson.Object obj -> Right (applyHtmlEscape obj, body)
      _ -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) "Expected Object"

parseTOML :: SPath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseTOML path raw body = 
  case Toml.parse raw of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right table -> 
      let obj = tableToAeson (Toml.forgetTableAnns table)
      in Right (applyHtmlEscape obj, body)

parseJSON :: SPath -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
parseJSON path raw body = 
  case Aeson.eitherDecodeStrict (T.encodeUtf8 raw) of
    Left err -> Left $ FrontmatterParseFailure path (SourcePos path 1 1) (T.pack err)
    Right obj -> Right (applyHtmlEscape obj, body)

applyHtmlEscape :: Aeson.Object -> Aeson.Object
applyHtmlEscape obj = case escapeHtmlValue (Aeson.Object obj) of
  Aeson.Object obj' -> obj'
  _ -> obj -- Should never happen

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
