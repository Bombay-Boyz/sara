{-# LANGUAGE OverloadedStrings #-}

module Unit.FrontmatterSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Frontmatter.Remap
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

spec :: Spec
spec = do
  describe "Frontmatter Parsing" $ do
    it "parses YAML frontmatter" $ do
      let input = "---\ntitle: Hello\n---\nBody"
      let res = parseFrontmatter "test.md" input
      case res of
        Right (meta, body) -> do
          KM.lookup (K.fromText "title") meta `shouldBe` Just (Aeson.String "Hello")
          body `shouldBe` "Body"
        Left _ -> expectationFailure "Failed to parse YAML"

    it "parses JSON frontmatter" $ do
      let input = "{\n  \"title\": \"Hello\"\n}\nBody"
      let res = parseFrontmatter "test.md" input
      case res of
        Right (meta, body) -> do
          KM.lookup (K.fromText "title") meta `shouldBe` Just (Aeson.String "Hello")
          body `shouldBe` "Body"
        Left _ -> expectationFailure "Failed to parse JSON"

  describe "Metadata Remapping" $ do
    it "remaps keys correctly" $ do
      let meta = KM.fromList [(K.fromText "author", Aeson.String "SARA")]
      let rules = [("author", "siteAuthor")]
      let (_, remapped) = remapMetadata rules "test.md" meta
      KM.lookup (K.fromText "siteAuthor") remapped `shouldBe` Just (Aeson.String "SARA")
