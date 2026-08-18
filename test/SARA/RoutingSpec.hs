{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module SARA.RoutingSpec (spec) where

import Test.Hspec
import SARA.Routing.Engine
import SARA.Routing.Types
import SARA.Error (SaraError(..))

spec :: Spec
spec = do
  describe "SARA.Routing.Engine" $ do
    describe "resolveRoute" $ do
      it "resolves SlugRoute" $ do
        let route = SlugRoute
        let source = "posts/hello.md"
        case resolveRoute route source of
          Right (ResolvedRoute path) -> path `shouldBe` "posts/hello.html"
          _ -> expectationFailure "SlugRoute resolution failed"
      
      it "resolves PrettyRoute" $ do
        let route = PrettyRoute
        let source = "posts/hello.md"
        case resolveRoute route source of
          Right (ResolvedRoute path) -> path `shouldBe` "posts/hello/index.html"
          _ -> expectationFailure "PrettyRoute resolution failed"

      it "rejects a LiteralRoute path containing a Windows-forbidden character" $ do
        -- '?' is a real, legal character in a Linux/macOS filename but
        -- is forbidden on Windows; a path resolving to one should be
        -- caught here rather than only breaking for a Windows user or
        -- CI runner later.
        case resolveRoute (LiteralRoute "posts/what?.html") "posts/what.md" of
          Left (RouteUnsafeForWindows p _) -> p `shouldBe` "posts/what?.html"
          other -> expectationFailure $ "Expected RouteUnsafeForWindows, got " ++ show other

      it "rejects a LiteralRoute path using a Windows-reserved device name" $ do
        case resolveRoute (LiteralRoute "posts/con.html") "posts/con.md" of
          Left (RouteUnsafeForWindows p _) -> p `shouldBe` "posts/con.html"
          other -> expectationFailure $ "Expected RouteUnsafeForWindows, got " ++ show other

      it "accepts an ordinary LiteralRoute path" $ do
        case resolveRoute (LiteralRoute "posts/ordinary-name.html") "posts/x.md" of
          Right (ResolvedRoute p) -> p `shouldBe` "posts/ordinary-name.html"
          other -> expectationFailure $ "Expected success, got " ++ show other

      it "reports a malformed RegexRoute replacement template as an error, rather than silently truncating or ignoring it" $ do
        -- The replacement ends in a lone, unescaped backslash with
        -- nothing after it — not a valid capture reference ('\N') and
        -- not an escaped backslash ('\\'), so it can't be interpolated
        -- at all. Before the 'eof' anchor fix, this would silently
        -- succeed with the trailing backslash dropped from the output.
        let result = regexRoute "^(posts)/(.*)\\.md$" "\\1/\\2.html\\"
        case result of
          Left _ -> expectationFailure "regexRoute itself should accept this pattern; the replacement is checked at resolveRoute time"
          Right route -> case resolveRoute route "posts/hello.md" of
            Left (RouteRegexInvalid _ _) -> pure ()
            other -> expectationFailure $ "Expected RouteRegexInvalid for a malformed replacement, got " ++ show other

      it "correctly interpolates a well-formed RegexRoute replacement (control case)" $ do
        let result = regexRoute "^(posts)/(.*)\\.md$" "\\1/\\2.html"
        case result of
          Left err -> expectationFailure $ "Expected a valid regex to be accepted, got " ++ show err
          Right route -> case resolveRoute route "posts/hello.md" of
            Right (ResolvedRoute p) -> p `shouldBe` "posts/hello.html"
            other -> expectationFailure $ "Expected successful interpolation, got " ++ show other
    
    describe "detectRouteConflicts" $ do
      it "detects conflicts between two routes" $ do
        let route = ResolvedRoute "output.html"
        let conflicts = detectRouteConflicts [("a.md", route), ("b.md", route)]
        length conflicts `shouldBe` 1
        case conflicts of
          [RouteConflict f1 f2 out] -> do
            f1 `shouldBe` "a.md"
            f2 `shouldBe` "b.md"
            out `shouldBe` "output.html"
          es -> expectationFailure $ "Expected exactly one RouteConflict, got " ++ show es
      
      it "passes with no conflicts" $ do
        let r1 = ResolvedRoute "a.html"
        let r2 = ResolvedRoute "b.html"
        let conflicts = detectRouteConflicts [("a.md", r1), ("b.md", r2)]
        length conflicts `shouldBe` 0
