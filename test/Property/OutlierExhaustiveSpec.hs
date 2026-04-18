{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

module Property.OutlierExhaustiveSpec (spec) where

import Test.Hspec
import SARA.Security.ShellGuard (validateArg)
import SARA.Security.PathGuard (guardPath, ProjectRoot(..))
import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Error (SaraError(..))
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T

success :: Expectation
success = return ()

spec :: Spec
spec = do
  describe "Outlier/Edge Case Exhaustive" $ do
    it "rejects shell arguments with forbidden characters" $ do
      let badArgs = ["hello world", "foo; bar", "baz'qux", "\"quoted\""]
      mapM_ (\a -> validateArg a `shouldSatisfy` (\case Left _ -> True; _ -> False)) badArgs
      
    it "rejects path traversal with nested dots" $ do
      let root = ProjectRoot "/app"
      res <- liftIO $ guardPath root "/app/./../etc/passwd"
      res `shouldSatisfy` (\case Left _ -> True; _ -> False)

    it "handles NUL bytes in frontmatter gracefully" $ do
      let input = "---\ntitle: \0\n---\nBody"
      case parseFrontmatter "test.md" (T.pack input) of
        Left (FrontmatterParseFailure _ _ _) -> success
        Right _ -> expectationFailure "NUL bytes in FM handled incorrectly"
        _ -> expectationFailure "Expected FrontmatterParseFailure"
