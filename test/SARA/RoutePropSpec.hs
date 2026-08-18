{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
module SARA.RoutePropSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import SARA.Routing.Types
import SARA.Routing.Engine
import SARA.Error (SaraError(..))

-- | Generator for Abstract Routes
instance Arbitrary (Route 'Abstract) where
  arbitrary = oneof 
    [ pure SlugRoute
    , pure PrettyRoute
    , LiteralRoute <$> elements ["a.html", "b.html", "c.html"]
    ]

-- | Property: for these three route kinds (no 'RegexRoute' here, since
--   its generator would need a 'SafeRegex' — covered separately by
--   'SARA.HedgehogSecuritySpec'), 'resolveRoute' either succeeds with a
--   'ResolvedRoute', or fails for exactly the one reason it's allowed
--   to: the resolved path being unsafe to write on Windows (see
--   'SARA.Routing.Engine.validatePortable'). Any other failure kind
--   would mean a new, undocumented way to fail crept in.
--
--   Strengthened from an earlier version of this property that claimed
--   'resolveRoute' always succeeds — a claim QuickCheck itself
--   falsified the moment a Windows-portability check was added, since
--   a generated 'FilePath' can (and, under this generator, sometimes
--   does) contain a character like @|@ that's legitimately forbidden
--   on Windows. That failure was correct, not a regression: the old
--   property was simply asserting an invariant that stopped being true
--   the moment a second, valid reason to fail was introduced.
prop_resolveRoute_isResolvedOrWindowsUnsafe :: Route 'Abstract -> FilePath -> Property
prop_resolveRoute_isResolvedOrWindowsUnsafe route path =
  case resolveRoute route path of
    Right (ResolvedRoute _)        -> property True
    Left (RouteUnsafeForWindows{}) -> property True
    Left other                     -> counterexample (show other) (property False)

spec :: Spec
spec = do
  describe "SARA Routing Properties" $ do
    it "resolveRoute always yields a ResolvedRoute, or fails only because the path is unsafe on Windows" $
      property prop_resolveRoute_isResolvedOrWindowsUnsafe
