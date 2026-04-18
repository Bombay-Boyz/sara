module Integration.SmokeSpec (spec) where

import Test.Hspec

import System.Directory (findExecutable)
import Data.Maybe (isJust)

spec :: Spec
spec = do
  describe "Smoke Test" $ do
    it "finds the cabal executable" $ do
      res <- findExecutable "cabal"
      isJust res `shouldBe` True
