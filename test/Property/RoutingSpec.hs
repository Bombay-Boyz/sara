{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Property.RoutingSpec (spec) where

import Test.Hspec
import SARA.Routing.Engine
import SARA.Types

spec :: Spec
spec = do
  describe "Routing Engine" $ do
    it "resolves SlugRoute to .html" $ do
        res <- resolveRoute SlugRoute "posts/hello-world.md"
        res `shouldBe` Right (ResolvedRoute "posts/hello-world.html")

    it "resolves PrettyRoute to index.html" $ do
        res <- resolveRoute PrettyRoute "posts/hello-world.md"
        res `shouldBe` Right (ResolvedRoute "posts/hello-world/index.html")

    it "detects conflicts when multiple files map to same output" $ do
        let routes = [ ("a.md", ResolvedRoute "same.html")
                     , ("b.md", ResolvedRoute "same.html")
                     ]
        detectRouteConflicts routes `shouldSatisfy` (not . null)

    it "permits distinct routes" $ do
        let routes = [ ("a.md", ResolvedRoute "a.html")
                     , ("b.md", ResolvedRoute "b.html")
                     ]
        detectRouteConflicts routes `shouldBe` []
