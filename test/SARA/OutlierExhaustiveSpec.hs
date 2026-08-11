{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.OutlierExhaustiveSpec (spec) where

import Test.Hspec
import SARA
import SARA.Frontmatter.Parser
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
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
        Left (FrontmatterParseFailure _ _ _) -> True `shouldBe` True
        _ -> expectationFailure "Should have failed with parse failure"

    it "handles mixed UTF-8 correctly" $ do
      let content = "---\ntitle: ⚡ SARA ⚡\n---\nこんにちは"
      case parseFrontmatter "utf8.md" content of
        Right (meta, body) -> do
          body `shouldBe` "こんにちは"
          KM.lookup "title" meta `shouldBe` Just (Aeson.String "⚡ SARA ⚡")
        Left e -> expectationFailure $ "UTF-8 should work: " ++ show e

    it "handles NUL bytes in frontmatter by rejecting or escaping" $ do
      -- The prior version of this test never forced the result ('let _
      -- = ...' is lazy and evaluates nothing), so it could not have
      -- caught a crash. Forcing the case here actually exercises the
      -- parser on NUL-byte input: either outcome (graceful rejection or
      -- an escaped/accepted value) is fine, but it must not throw, and
      -- if accepted, the raw NUL must not survive into a parsed value.
      let content = "---\ntitle: \0\n---"
      case parseFrontmatter "nul.md" content of
        Left _ -> pure () -- graceful rejection is an acceptable outcome
        Right (meta, _) ->
          case KM.lookup "title" meta of
            Just (Aeson.String t) -> t `shouldNotSatisfy` T.any (== '\0')
            _ -> pure () -- absent or non-string title is also acceptable

    it "rejects paths with NUL bytes (ShellGuard)" $ do
      case validateArg "path\0with\0nul" of
        Left (SecurityShellInjection _ _) -> True `shouldBe` True
        _ -> expectationFailure "Should have rejected NUL in path"
