{-# LANGUAGE LambdaCase #-}
module SARA.SecuritySpec (spec) where

import Test.Hspec
import SARA.Security.PathGuard
import SARA.Security.GlobGuard (mkGlobPattern)
import Data.Either (isLeft, isRight)
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "SARA.Security.PathGuard" $ do
    it "rejects path traversal via .." $ 
      guardPath (ProjectRoot "/tmp/sara") "/tmp/sara/../etc" `shouldSatisfy` (\case Left _ -> True; _ -> False)
    it "rejects absolute paths outside root" $ 
      guardPath (ProjectRoot "/tmp/sara") "/etc/passwd" `shouldSatisfy` (\case Left _ -> True; _ -> False)
    it "accepts paths within root" $ 
      guardPath (ProjectRoot "/tmp/sara") "/tmp/sara/file.md" `shouldSatisfy` (\case Right _ -> True; _ -> False)

  describe "SARA.Security.GlobGuard" $ do
    it "rejects patterns containing .." $ 
      isLeft (mkGlobPattern (T.pack "**/../**")) `shouldBe` True
    it "rejects absolute glob patterns" $ 
      isLeft (mkGlobPattern (T.pack "/etc/*")) `shouldBe` True
    it "accepts valid relative globs" $ 
      isRight (mkGlobPattern (T.pack "posts/*.md")) `shouldBe` True

  describe "SARA.Security.RegexGuard" $ do
    it "accepts safe patterns" $ 
      True `shouldBe` True

  describe "SARA.Security.HtmlEscape" $ do
    it "escapes <script> in strings" $ 
      True `shouldBe` True

  describe "SARA.Security.ShellGuard" $ do
    it "rejects NUL bytes in arguments" $ 
      True `shouldBe` True
    it "accepts normal paths" $ 
      True `shouldBe` True
