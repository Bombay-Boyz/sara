{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Unit.FrontmatterSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Detect
import SARA.Frontmatter.Parser
import SARA.Frontmatter.Remap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "SARA.Frontmatter" $ do
    describe "Frontmatter Detection" $ do
      it "detects YAML" $ do
        detectFormat "---\ntitle: YAML\n---" `shouldBe` FmYAML
      it "detects TOML" $ do
        detectFormat "+++\ntitle = \"TOML\"\n+++" `shouldBe` FmTOML
      it "detects JSON" $ do
        detectFormat "{\n\"title\": \"JSON\"\n}" `shouldBe` FmJSON
      it "handles none" $ do
        detectFormat "No frontmatter here" `shouldBe` FmNone

    describe "Frontmatter Parsing" $ do
      it "parses YAML and escapes HTML" $ do
        let content = "---\ntitle: <script>alert(1)</script>\n---"
        case parseFrontmatter "test.md" content of
          Right (meta, _) -> KM.lookup (K.fromText "title") meta `shouldBe` Just (Aeson.String "&lt;script&gt;alert(1)&lt;/script&gt;")
          Left _ -> expectationFailure "Should parse valid YAML"

      it "parses JSON" $ do
        let content = "{\"title\": \"JSON\"}"
        case parseFrontmatter "test.md" content of
          Right (meta, _) -> KM.lookup (K.fromText "title") meta `shouldBe` Just (Aeson.String "JSON")
          Left _ -> expectationFailure "Should parse valid JSON"

    describe "Metadata Remapping" $ do
      it "remaps keys" $ do
        let meta = KM.fromList [ (K.fromText "fromKey", Aeson.String "value") ]
        case remapMetadata [("fromKey", "toKey")] "test.md" meta of
          Right m -> do
            KM.lookup (K.fromText "toKey") m `shouldBe` Just (Aeson.String "value")
            KM.member (K.fromText "fromKey") m `shouldBe` False
          Left e -> expectationFailure $ "Remap failed: " ++ show e

      it "succeeds (no-op) if fromKey is missing" $ do
        let meta = KM.fromList [ (K.fromText "title", Aeson.String "Test") ]
        case remapMetadata [("missing", "new")] "test.md" meta of
          Right m -> do
            KM.member (K.fromText "title") m `shouldBe` True
            KM.member (K.fromText "new") m `shouldBe` False
          Left e -> expectationFailure $ "Should not fail on missing key: " ++ show e
