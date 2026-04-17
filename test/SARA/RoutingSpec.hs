{-# LANGUAGE GADTs #-}
module SARA.RoutingSpec (spec) where

import Test.Hspec
import SARA.Routing.Engine
import SARA.Routing.Types
import SARA.Routing.Error

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
    
    describe "detectRouteConflicts" $ do
      it "detects conflicts between two routes" $ do
        let route = ResolvedRoute "output.html"
        let conflicts = detectRouteConflicts [("a.md", route), ("b.md", route)]
        length conflicts `shouldBe` 1
        case head conflicts of
          RouteConflict f1 f2 out -> do
            f1 `shouldBe` "a.md"
            f2 `shouldBe` "b.md"
            out `shouldBe` "output.html"
          e -> expectationFailure $ "Expected RouteConflict, got " ++ show e
      
      it "passes with no conflicts" $ do
        let r1 = ResolvedRoute "a.html"
        let r2 = ResolvedRoute "b.html"
        let conflicts = detectRouteConflicts [("a.md", r1), ("b.md", r2)]
        length conflicts `shouldBe` 0
