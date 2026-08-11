{-# LANGUAGE OverloadedStrings #-}

module SARA.Asset.Copy
  ( copyAsset
  ) where

import Development.Shake
import SARA.Security.PathGuard

-- | Zero-copy asset passthrough using Shake's copyFile'
copyAsset
  :: SafePath -- ^ Source
  -> SafePath -- ^ Destination
  -> Action ()
copyAsset (SafePath src) (SafePath dst) = copyFile' src dst
