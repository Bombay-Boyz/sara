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

-- | Property: Parsing and re-encoding simple YAML should be an isomorphism.
-- Restricted to alphanumeric to avoid HTML-escape mismatch.
prop_yaml_roundtrip :: [(String, String)] -> Property
prop_yaml_roundtrip pairs =
  let cleanPairs = filter (\(k, v) -> all isAlphaNum k && all isAlphaNum v && not (null k)) pairs
      obj = Aeson.object [ Key.fromString k Aeson..= v | (k, v) <- cleanPairs ]
      raw = T.decodeUtf8 (Yaml.encode obj)
      content = T.pack "---\n" <> raw <> T.pack "---\n"
  in case parseFrontmatter "test.md" content of
       Right (meta, _) -> Aeson.toJSON meta === Aeson.toJSON obj
       Left _          -> property False
  where
    isAlphaNum c = c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c >= '0' && c <= '9'

spec :: Spec
spec = do
  describe "SARA Frontmatter Properties" $ do
    it "YAML roundtrip is an isomorphism for simple objects" $
      property prop_yaml_roundtrip
