{-# LANGUAGE LambdaCase #-}
module SARA.OutlierSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Security.PathGuard (ProjectRoot(..), guardPath)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Outliers and Edge Cases" $ do
    describe "Frontmatter Parsing" $ do
      it "handles multi-byte UTF-8 characters across delimiters" $ do
        let content = T.pack "---\nkey: 🦀\n---\n"
        case parseFrontmatter "test.md" content of
          Right (meta, _) -> show meta `shouldContain` "key"
          Left err -> expectationFailure $ "Failed to parse: " ++ show err
      
      it "handles files with only frontmatter and no body" $ do
        let content = T.pack "---\ntitle: OnlyMeta\n---\n"
        case parseFrontmatter "test.md" content of
          Right (_, body) -> T.null body `shouldBe` True
          Left err -> expectationFailure $ "Failed to parse: " ++ show err

    describe "Security Edge Cases" $ do
      it "handles path with NUL bytes gracefully (PathGuard does not throw, and does not accept the path)" $ do
        -- A NUL byte can't appear in a real POSIX path; a robust guard
        -- must reject it deterministically rather than throwing or,
        -- worse, treating it as if it were within the project root.
        guardPath (ProjectRoot "/tmp/sara") "/tmp/sara/evil\0file"
          `shouldSatisfy` (\case Left _ -> True; Right _ -> False)

    describe "Large File Handling (Stress Test)" $ do
      it "processes 1000 frontmatter keys efficiently" $ do
        let keys = [T.pack $ "k" ++ show i | i <- [1..1000 :: Int]]
        let fm = T.unlines $ map (\k -> k <> T.pack ": value") keys
        let content = T.pack "---\n" <> fm <> T.pack "---\n"
        case parseFrontmatter "large.md" content of
          Right (_, _) -> True `shouldBe` True
          Left err -> expectationFailure $ "Failed to parse large fm: " ++ show err
