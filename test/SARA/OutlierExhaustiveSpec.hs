{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.OutlierExhaustiveSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Security.ShellGuard (validatePath, validateArg)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as BS
import SARA.Error (SaraError(..), AnySaraError(..))

spec :: Spec
spec = do
  describe "SARA Outlier & Edge Case Exhaustive" $ do
    it "handles empty files" $ do
      case parseFrontmatter "empty.md" "" of
        Right (meta, body) -> do
          show meta `shouldBe` "fromList []"
          T.null body `shouldBe` True
        Left _ -> expectationFailure "Empty file should parse as empty FM"

    it "handles files with ONLY frontmatter" $ do
      let content = "---\ntitle: Only FM\n---"
      case parseFrontmatter "onlyfm.md" content of
        Right (meta, body) -> do
          show meta `shouldContain` "Only FM"
          T.null body `shouldBe` True
        Left _ -> expectationFailure "File with only FM should parse body as empty"

    it "handles malformed YAML gracefully" $ do
      let content = "---\nkey: : invalid\n---"
      case parseFrontmatter "malformed.md" content of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "Malformed YAML should return Left error"

    it "handles mixed UTF-8 correctly" $ do
      let content = "---\ntitle: 🚀\n---\nValid body"
      case parseFrontmatter "mixed.md" content of
        Right (meta, body) -> do
          KM.lookup (K.fromText "title") meta `shouldBe` Just (Aeson.String "🚀")
          body `shouldBe` "Valid body"
        Left _ -> expectationFailure "Mixed UTF-8 should parse correctly"

    it "handles NUL bytes in frontmatter by rejecting or escaping" $ do
      let content = "---\nkey: value\0\n---"
      -- parseFrontmatter should not crash
      case parseFrontmatter "nul.md" content of
        Right _ -> True `shouldBe` True
        Left (FrontmatterParseFailure _ _ _) -> True `shouldBe` True
        Left (FrontmatterUnknownFormat _) -> True `shouldBe` True
        Left (FrontmatterRemapMissing _ _) -> True `shouldBe` True
        _ -> expectationFailure "NUL bytes in FM handled incorrectly"

    it "rejects paths with NUL bytes (ShellGuard)" $ do
      let path = "sneaky\0.md"
      case validateArg path of
        Left _ -> True `shouldBe` True
        Right _ -> expectationFailure "ShellGuard should reject paths with NUL bytes"
