{-# LANGUAGE OverloadedStrings #-}

module SARA.PaginationSpec (spec) where

import Test.Hspec
import SARA.Content.Pagination (paginate)

spec :: Spec
spec = describe "SARA.Content.Pagination" $ do
  describe "paginate" $ do
    it "splits a list into chunks of the given size" $
      paginate 2 [1, 2, 3, 4, 5 :: Int] `shouldBe` [[1, 2], [3, 4], [5]]

    it "returns one page when the list is smaller than the page size" $
      paginate 10 [1, 2, 3 :: Int] `shouldBe` [[1, 2, 3]]

    it "returns no pages for an empty list" $
      paginate 5 ([] :: [Int]) `shouldBe` []

    it "returns exactly-sized pages when the list divides evenly" $
      paginate 2 [1, 2, 3, 4 :: Int] `shouldBe` [[1, 2], [3, 4]]

    it "is total, not a hang or a crash, for a page size of zero (clamped to 1 — see Haddock)" $
      paginate 0 [1, 2, 3 :: Int] `shouldBe` [[1], [2], [3]]

    it "is total for a negative page size (clamped to 1)" $
      paginate (-5) [1, 2 :: Int] `shouldBe` [[1], [2]]
