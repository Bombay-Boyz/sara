{-# LANGUAGE GADTs #-}
module SARA.RoutingSpec (spec) where

import Test.Hspec
import SARA.Types
import SARA.Routing.Engine
import SARA.Error

spec :: Spec
spec = do
  describe "SARA.Routing.Engine" $ do
    describe "resolveRoute" $ do
      it "resolves SlugRoute" $ do
        res <- resolveRoute SlugRoute "posts/hello-world.md"
        res `shouldBe` Right (ResolvedRoute "posts/hello-world.html")

      it "resolves PrettyRoute" $ do
        res <- resolveRoute PrettyRoute "posts/hello-world.md"
        res `shouldBe` Right (ResolvedRoute "posts/hello-world/index.html")

    describe "detectRouteConflicts" $ do
      it "detects conflicts between two routes" $ do
        let routes = [ ("a.md", ResolvedRoute "same.html")
                     , ("b.md", ResolvedRoute "same.html")
                     ]
        let conflicts = detectRouteConflicts routes
        length conflicts `shouldBe` 1

      it "passes with no conflicts" $ do
        let routes = [ ("a.md", ResolvedRoute "a.html")
                     , ("b.md", ResolvedRoute "b.html")
                     ]
        let conflicts = detectRouteConflicts routes
        length conflicts `shouldBe` 0
