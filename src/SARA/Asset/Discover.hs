{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.Asset.Discover
  ( discoverAssets
  , inferAssetKind
  ) where

import SARA.Types
import SARA.Monad
import SARA.Config (SaraConfig(..))
import System.FilePath (takeExtension)
import qualified Data.Text as T
import Data.Maybe (mapMaybe)

-- | Glob-based auto-discovery.
discoverAssets
  :: GlobPattern
  -> SaraM ()
discoverAssets g = tellRule (RuleDiscover g)

-- | Infers kind based on extension and configuration.
inferAssetKind :: SaraConfig -> SPath -> SomeAssetKind
inferAssetKind cfg pathText = 
  let path = T.unpack pathText
      fmts = mapMaybe formatFromText (cfgDefaultImageFormats cfg)
      spec = ImageSpec [] fmts (cfgDefaultImageQuality cfg)
  in case takeExtension path of
    ".png"   -> SomeAssetKind (ImageAsset spec)
    ".jpg"   -> SomeAssetKind (ImageAsset spec)
    ".jpeg"  -> SomeAssetKind (ImageAsset spec)
    ".webp"  -> SomeAssetKind (ImageAsset spec)
    ".avif"  -> SomeAssetKind (ImageAsset spec)
    ".css"   -> SomeAssetKind StyleAsset
    ".js"    -> SomeAssetKind ScriptAsset
    ".mjs"   -> SomeAssetKind ScriptAsset
    ".woff"  -> SomeAssetKind FontAsset
    ".woff2" -> SomeAssetKind FontAsset
    ".ttf"   -> SomeAssetKind FontAsset
    ".json"  -> SomeAssetKind DataAsset
    ".xml"   -> SomeAssetKind DataAsset
    _        -> SomeAssetKind GenericAsset
