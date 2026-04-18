{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Property.RoutePropSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic
import SARA.Types
import SARA.Routing.Engine

instance Arbitrary (Route 'Abstract) where
  arbitrary = oneof 
    [ pure SlugRoute
    , pure PrettyRoute
    , LiteralRoute <$> elements ["a.html", "b.html", "c.html"]
    ]

-- | Property: resolveRoute always returns a ResolvedRoute
prop_resolveRoute_isResolved :: Route 'Abstract -> FilePath -> Property
prop_resolveRoute_isResolved route path = monadicIO $ do
  res <- run $ resolveRoute route path
  case res of
    Right (ResolvedRoute _) -> assert True
    _                       -> assert False

spec :: Spec
spec = do
  describe "SARA Routing Properties" $ do
    it "resolveRoute always yields a ResolvedRoute" $
      property prop_resolveRoute_isResolved
