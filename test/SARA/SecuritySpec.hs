{-# LANGUAGE LambdaCase #-}
module SARA.SecuritySpec (spec) where

import Test.Hspec
import SARA.Security.PathGuard
import SARA.Security.GlobGuard (mkGlobPattern)
import SARA.Security.RegexGuard (mkSafeRegex, unSafeRegex)
import SARA.Security.HtmlEscape (escapeHtmlValue)
import SARA.Security.ShellGuard (validateArg)
import Data.Either (isLeft, isRight)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
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
      -- A plain, unquantified-group pattern has no nested quantifiers,
      -- no alternation-in-repetition, and shallow nesting depth, so it
      -- must compile and pass every heuristic in 'checkComplexity'.
      case mkSafeRegex (T.pack "^posts/[a-z0-9-]+\\.md$") of
        Right safe -> unSafeRegex safe `shouldBe` T.pack "^posts/[a-z0-9-]+\\.md$"
        Left err   -> expectationFailure $ "Expected a safe pattern to be accepted: " ++ show err

    it "rejects nested quantifiers (classic ReDoS shape)" $
      isLeft (mkSafeRegex (T.pack "(a+)+")) `shouldBe` True

    it "rejects alternation inside unbounded repetition" $
      isLeft (mkSafeRegex (T.pack "(a|ab)+")) `shouldBe` True

    it "rejects patterns that fail to compile" $
      isLeft (mkSafeRegex (T.pack "(unclosed")) `shouldBe` True

  describe "SARA.Security.HtmlEscape" $ do
    it "escapes <script> in strings" $
      escapeHtmlValue (Aeson.String (T.pack "<script>alert(1)</script>"))
        `shouldBe` Aeson.String (T.pack "&lt;script&gt;alert(1)&lt;/script&gt;")

    it "escapes the five reserved HTML characters (<, >, &, \", ')" $
      escapeHtmlValue (Aeson.String (T.pack "<>&\"'"))
        `shouldBe` Aeson.String (T.pack "&lt;&gt;&amp;&quot;&#39;")

    it "recurses into nested objects and arrays, leaving non-strings untouched" $
      let input = Aeson.Object $ KM.fromList
            [ (K.fromText (T.pack "title"), Aeson.String (T.pack "<b>hi</b>"))
            , (K.fromText (T.pack "tags"), Aeson.Array (pure (Aeson.String (T.pack "<i>x</i>"))))
            , (K.fromText (T.pack "count"), Aeson.Number 3)
            ]
          expected = Aeson.Object $ KM.fromList
            [ (K.fromText (T.pack "title"), Aeson.String (T.pack "&lt;b&gt;hi&lt;/b&gt;"))
            , (K.fromText (T.pack "tags"), Aeson.Array (pure (Aeson.String (T.pack "&lt;i&gt;x&lt;/i&gt;"))))
            , (K.fromText (T.pack "count"), Aeson.Number 3)
            ]
      in escapeHtmlValue input `shouldBe` expected

  describe "SARA.Security.ShellGuard" $ do
    it "rejects NUL bytes in arguments" $
      isLeft (validateArg "posts/evil\0file.md") `shouldBe` True

    it "accepts normal paths" $
      isRight (validateArg "posts/normal-file.md") `shouldBe` True
