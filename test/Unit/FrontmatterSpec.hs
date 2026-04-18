{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Unit.FrontmatterSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Detect
import SARA.Frontmatter.Parser
import qualified SARA.Frontmatter.Remap as Remap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T
import SARA.Error

spec :: Spec
spec = do
  describe "SARA.Frontmatter" $ do
    describe "Frontmatter Detection" $ do
      it "detects YAML" $ 
        detectFormat "---\ntitle: test\n---" `shouldBe` FmYAML
      it "detects TOML" $ 
        detectFormat "+++\ntitle = \"test\"\n+++" `shouldBe` FmTOML
      it "detects JSON" $ 
        detectFormat "{\n\"title\": \"test\"\n}" `shouldBe` FmJSON
      it "handles none" $ do
        let content = "Just markdown"
        detectFormat content `shouldBe` FmNone
        case splitFrontmatter FmNone content of
          Right (fm, body) -> do
            fm `shouldBe` ""
            body `shouldBe` "Just markdown"
          Left e -> expectationFailure $ "FmNone should not fail: " ++ show e

    describe "Frontmatter Parsing" $ do
      it "parses YAML and escapes HTML" $ do
        let content = "---\ntitle: <script>alert(1)</script>\n---"
        case parseFrontmatter "test.md" content of
          Right (meta, _) -> 
            KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String "&lt;script&gt;alert(1)&lt;/script&gt;")
          Left e -> expectationFailure $ "YAML parse failed: " ++ show e

      it "parses JSON" $ do
        let content = "{\n\"title\": \"JSON\"\n}\nBody"
        case parseFrontmatter "test.md" content of
          Right (meta, body) -> do
            KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String "JSON")
            body `shouldBe` "\nBody"
          Left e -> expectationFailure $ "JSON parse failed: " ++ show e

    describe "Metadata Remapping" $ do
      it "remaps keys" $ do
        let meta = KM.fromList [(Key.fromString "from", Aeson.String (T.pack "value"))]
        let rules = [("from", "to")]
        case Remap.remapMetadata rules "test.md" meta of
          Right remapped -> KM.lookup (Key.fromString "to") remapped `shouldBe` Just (Aeson.String (T.pack "value"))
          Left e -> expectationFailure $ "Remap failed: " ++ show e
        
      it "fails if fromKey is missing" $ do
        let meta = KM.empty
        let rules = [("missing", "to")]
        let res = Remap.remapMetadata rules "test.md" meta
        case res of
          Left (FrontmatterRemapMissing _ _) -> True `shouldBe` True
          _ -> expectationFailure "Should have failed with remap missing error"
