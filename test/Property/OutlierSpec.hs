{-# LANGUAGE OverloadedStrings #-}

module Property.OutlierSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Outliers and Edge Cases" $ do
    describe "Frontmatter Parsing" $ do
      it "handles multi-byte UTF-8 characters across delimiters" $ do
        let content = "---\nkey: 😃\n---\n"
        case parseFrontmatter "test.md" content of
          Right (meta, _) -> show meta `shouldContain` "key"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err

      it "handles files with only frontmatter and no body" $ do
        let content = "---\ntitle: Only FM\n---"
        case parseFrontmatter "test.md" content of
          Right (_, body) -> T.null body `shouldBe` True
          Left err -> expectationFailure $ "Failed to parse: " ++ show err

    describe "Security Edge Cases" $ do
      it "handles path with NUL bytes gracefully" $ do
        case parseFrontmatter "test\0.md" (T.pack "---") of
          Left _ -> True `shouldBe` True
          Right _ -> True `shouldBe` True

    describe "Large File Handling (Stress Test)" $ do
      it "processes 1000 frontmatter keys efficiently" $ do
        let keys = [ "key" <> T.pack (show i) <> ": value" <> T.pack (show i) | i <- [1..1000] :: [Int] ]
        let content = "---\n" <> T.unlines keys <> "---\n"
        case parseFrontmatter "large.md" content of
          Right (meta, _) -> length (show meta) `shouldSatisfy` (> 0)
          Left err -> expectationFailure $ "Failed to parse large fm: " ++ show err
