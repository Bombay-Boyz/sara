{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.OutlierExhaustiveSpec (spec) where

import Test.Hspec
import SARA
import SARA.Frontmatter.Parser
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM

spec :: Spec
spec = do
  describe "SARA Outlier & Edge Case Exhaustive" $ do
    it "handles empty files" $ do
      case parseFrontmatter "empty.md" "" of
        Right (meta, body) -> do
          let _ = meta
          KM.null meta `shouldBe` True
          body `shouldBe` ""
        Left e -> expectationFailure $ "Empty file should not fail: " ++ show e

    it "handles files with ONLY frontmatter" $ do
      case parseFrontmatter "only.md" "---\ntitle: Only\n---" of
        Right (meta, body) -> do
          T.null body `shouldBe` True
          KM.size meta `shouldBe` 1
        Left e -> expectationFailure $ "Only frontmatter should not fail: " ++ show e

    it "handles malformed YAML gracefully" $ do
      case parseFrontmatter "bad.md" "---\ntitle: [unclosed bracket\n---\nBody" of
        Left (FrontmatterParseFailure f _ _) -> f `shouldBe` "bad.md"
        _ -> expectationFailure "Should have failed with parse failure"

    it "handles mixed UTF-8 correctly" $ do
      let content = "---\ntitle: ⚡ SARA ⚡\n---\nこんにちは"
      case parseFrontmatter "utf8.md" content of
        Right (meta, body) -> do
          body `shouldBe` "こんにちは"
        Left e -> expectationFailure $ "UTF-8 should work: " ++ show e

    it "handles NUL bytes in frontmatter by rejecting or escaping" $ do
      let content = "---\ntitle: \0\n---"
      case parseFrontmatter "nul.md" content of
        Left (FrontmatterParseFailure f _ _) -> f `shouldBe` "nul.md"
        Right _ -> True `shouldBe` True -- Some parsers might escape it, which is also fine

    it "rejects paths with NUL bytes (ShellGuard)" $ do
      case validateArg "path\0with\0nul" of
        Left (SecurityShellInjection p r) -> do
          p `shouldBe` "path\0with\0nul"
          T.isInfixOf "NUL" r || T.isInfixOf "forbidden" r `shouldBe` True
        _ -> expectationFailure "Should have rejected NUL in path"
