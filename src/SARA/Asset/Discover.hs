{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.Asset.Discover
  ( discoverAssets
  , inferAssetKind
  ) where

import SARA.Types
import SARA.Monad
import System.FilePath (takeExtension)
import Control.Monad.Writer (tell)

-- | Glob-based auto-discovery.
discoverAssets
  :: GlobPattern
  -> SaraM ()
discoverAssets g = tell [RuleDiscover g]

-- | Infers kind based on extension.
inferAssetKind :: FilePath -> SomeAssetKind
inferAssetKind path = case takeExtension path of
  ".png"   -> SomeAssetKind (ImageAsset (ImageSpec [] [PNG] 80))
  ".jpg"   -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80))
  ".jpeg"  -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80))
  ".css"   -> SomeAssetKind StyleAsset
  ".js"    -> SomeAssetKind ScriptAsset
  ".mjs"   -> SomeAssetKind ScriptAsset
  ".woff"  -> SomeAssetKind FontAsset
  ".woff2" -> SomeAssetKind FontAsset
  ".ttf"   -> SomeAssetKind FontAsset
  ".json"  -> SomeAssetKind DataAsset
  ".xml"   -> SomeAssetKind DataAsset
  _        -> SomeAssetKind GenericAsset
