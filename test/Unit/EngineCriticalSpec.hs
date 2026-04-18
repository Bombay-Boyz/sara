{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Unit.EngineCriticalSpec (spec) where

import Test.Hspec
import SARA.Monad
import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  describe "Engine Critical Infrastructure" $ do
    it "initializes state with empty caches" $ do
      let s = initialState
      Map.null (stateItemCache s) `shouldBe` True
      Map.null (stateTemplateCache s) `shouldBe` True
