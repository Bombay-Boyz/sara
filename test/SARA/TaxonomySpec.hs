{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.TaxonomySpec (spec) where

import Test.Hspec
import SARA.Content.Taxonomy
import SARA.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.Vector as V

mkItem :: FilePath -> Aeson.Object -> ItemP 'Validated Aeson.Object
mkItem path meta = Item
  { itemPath  = path
  , itemRoute = ResolvedRoute (path ++ ".html")
  , itemMeta  = meta
  , itemBody  = "body"
  , itemHash  = "0000000000000000000000000000000000000000000000000000000000000000"
  }

spec :: Spec
spec = describe "SARA.Content.Taxonomy" $ do
  describe "slugify" $ do
    it "lowercases and hyphenates" $
      slugify "Haskell Tips" `shouldBe` "haskell-tips"

    it "collapses runs of non-alphanumeric characters into one hyphen" $
      slugify "C++ & Rust!!" `shouldBe` "c-rust"

    it "trims leading/trailing hyphens" $
      slugify "  spaced out  " `shouldBe` "spaced-out"

    it "is total (does not crash) on an empty string" $
      slugify "" `shouldBe` ""

  describe "extractTerms" $ do
    it "extracts every string from a JSON array field" $
      extractTerms "tags" (mkItem "a" (KM.fromList [("tags", Aeson.Array (V.fromList (map Aeson.String ["haskell", "ssg"])))]))
        `shouldBe` ["haskell", "ssg"]

    it "extracts a single term from a bare string field" $
      extractTerms "category" (mkItem "a" (KM.fromList [("category", Aeson.String "tutorials")]))
        `shouldBe` ["tutorials"]

    it "extracts nothing when the field is absent" $
      extractTerms "tags" (mkItem "a" KM.empty) `shouldBe` []

    it "extracts nothing (not a crash or a guess) when the field has an unexpected shape" $
      extractTerms "tags" (mkItem "a" (KM.fromList [("tags", Aeson.Number 5)])) `shouldBe` []

  describe "groupByTerm" $ do
    it "groups items under every tag they carry, including sharing one item across multiple tags" $ do
      let itemA = mkItem "a" (KM.fromList [("tags", Aeson.Array (V.fromList (map Aeson.String ["haskell", "ssg"])))])
      let itemB = mkItem "b" (KM.fromList [("tags", Aeson.Array (V.fromList (map Aeson.String ["haskell"])))])
      let grouped = groupByTerm "tags" [itemA, itemB]
      (map itemPath <$> Map.lookup "haskell" grouped) `shouldBe` Just ["b", "a"]
      (map itemPath <$> Map.lookup "ssg" grouped) `shouldBe` Just ["a"]
      Map.member "nonexistent" grouped `shouldBe` False
