{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module SARA.PropertySpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SARA.Frontmatter.Parser
import qualified Data.Text as T

-- | Property: Parsing any YAML-like string that starts with --- should not crash
prop_parse_no_crash :: String -> Property
prop_parse_no_crash s =
  let content = T.pack ("---\n" <> s <> "\n---")
  in case parseFrontmatter "test.md" content of
       Right _ -> property True
       Left _  -> property True

spec :: Spec
spec = do
  describe "SARA Property Tests" $ do
    it "Parsing any string delimited by --- does not crash" $
      property prop_parse_no_crash
