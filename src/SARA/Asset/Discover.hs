{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.Asset.Discover
  ( discoverAssets
  , inferAssetKind
  ) where

import SARA.Types
import SARA.Monad
import System.FilePath (takeExtension)
import qualified Data.Text as T

-- | Glob-based auto-discovery.
discoverAssets
  :: GlobPattern
  -> SaraM ()
discoverAssets g = tellRule (RuleDiscover g)

-- | Infers kind based on extension.
inferAssetKind :: SPath -> SomeAssetKind
inferAssetKind pathText = 
  let path = T.unpack pathText
  in case takeExtension path of
    ".png"   -> SomeAssetKind (ImageAsset (ImageSpec [] [PNG] 80))
    ".jpg"   -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80))
    ".jpeg"  -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80))
    ".webp"  -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80)) -- Assume JPEG as fallback for spec if needed
    ".avif"  -> SomeAssetKind (ImageAsset (ImageSpec [] [JPEG] 80))
    ".css"   -> SomeAssetKind StyleAsset
    ".js"    -> SomeAssetKind ScriptAsset
    ".mjs"   -> SomeAssetKind ScriptAsset
    ".woff"  -> SomeAssetKind FontAsset
    ".woff2" -> SomeAssetKind FontAsset
    ".ttf"   -> SomeAssetKind FontAsset
    ".json"  -> SomeAssetKind DataAsset
    ".xml"   -> SomeAssetKind DataAsset
    _        -> SomeAssetKind GenericAsset
