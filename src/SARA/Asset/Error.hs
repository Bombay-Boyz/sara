{-# LANGUAGE GADTs #-}

module SARA.Asset.Error
  ( AssetError(..)
  ) where

import Data.Text (Text)

data AssetError where
  AssetProcessingFailed
    :: { astFile :: !FilePath, astDetail :: !Text }
    -> AssetError

deriving instance Show AssetError
