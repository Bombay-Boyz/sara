{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module SARA.RoutePropSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SARA.Routing.Types
import SARA.Routing.Engine

-- | Generator for Abstract Routes
instance Arbitrary (Route 'Abstract) where
  arbitrary = oneof 
    [ pure SlugRoute
    , pure PrettyRoute
    , LiteralRoute <$> elements ["a.html", "b.html", "c.html"]
    ]

-- | Property: resolveRoute always returns a ResolvedRoute
prop_resolveRoute_isResolved :: Route 'Abstract -> FilePath -> Property
prop_resolveRoute_isResolved route path =
  case resolveRoute route path of
    Right (ResolvedRoute _) -> property True
    _                       -> property False

spec :: Spec
spec = do
  describe "SARA Routing Properties" $ do
    it "resolveRoute always yields a ResolvedRoute" $
      property prop_resolveRoute_isResolved
