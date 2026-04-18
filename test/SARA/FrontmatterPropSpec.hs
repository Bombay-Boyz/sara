{-# LANGUAGE OverloadedStrings #-}

module SARA.FrontmatterPropSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SARA.Frontmatter.Parser
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Yaml as Yaml
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import SARA.Security.HtmlEscape (escapeHtmlValue)

-- | Property: Parsing and re-encoding simple YAML should be an isomorphism.
--   Now includes special characters to verify HTML escaping.
prop_yaml_roundtrip :: [(String, String)] -> Property
prop_yaml_roundtrip pairs =
  let cleanPairs = filter (\(k, v) -> not (null k) && all (`notElem` (":\n" :: String)) k && not ('\n' `elem` v)) pairs
      obj = Aeson.object [ Key.fromString k Aeson..= v | (k, v) <- cleanPairs ]
      -- We expect the parser to HTML-escape values
      expected = escapeHtmlValue obj
      raw = T.decodeUtf8 (Yaml.encode obj)
      content = T.pack "---\n" <> raw <> T.pack "---\n"
  in case parseFrontmatter "test.md" content of
       Right (meta, _) -> Aeson.toJSON meta === Aeson.toJSON expected
       Left _          -> property False

spec :: Spec
spec = do
  describe "SARA Frontmatter Properties" $ do
    it "YAML roundtrip is an isomorphism for simple objects (including special chars)" $
      property prop_yaml_roundtrip
