module SARA.SmokeSpec (spec) where

import Test.Hspec

spec :: Spec
spec = do
  describe "Smoke Test" $ do
    it "passes" $ do
      True `shouldBe` True
