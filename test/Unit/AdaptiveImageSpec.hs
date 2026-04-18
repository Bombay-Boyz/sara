{-# LANGUAGE OverloadedStrings #-}

module Unit.AdaptiveImageSpec (spec) where

import Test.Hspec
import SARA.Markdown.Shortcode
import qualified Data.Map.Strict as Map
import Data.Functor.Identity (runIdentity)

spec :: Spec
spec = do
  describe "Adaptive Image Shortcodes" $ do
    
    it "parses key-value args correctly" $ do
      let input = "{{% image src=\"photo.jpg\" alt=\"A sunset\" %}}"
      let res = parseShortcodes input
      length res `shouldBe` 1
      let sc = head res
      scName sc `shouldBe` "image"
      Map.lookup "src" (scArgs sc) `shouldBe` Just "photo.jpg"
      Map.lookup "alt" (scArgs sc) `shouldBe` Just "A sunset"

    it "expands shortcodes into HTML picture tags" $ do
      let input = "Check this: {{% image src=\"sunset.jpg\" alt=\"Sunset\" %}}"
      let handler sc = return $ if scName sc == "image"
            then "<picture><img src=\"" <> (Map.findWithDefault "" "src" (scArgs sc)) <> "\"></picture>"
            else ""
      let expanded = runIdentity $ expandShortcodes handler input
      expanded `shouldBe` "Check this: <picture><img src=\"sunset.jpg\"></picture>"

    it "handles multiple shortcodes in one block" $ do
      let input = "{{% a %}} and {{% b %}}"
      let handler sc = return $ "[" <> scName sc <> "]"
      runIdentity (expandShortcodes handler input) `shouldBe` "[a] and [b]"
