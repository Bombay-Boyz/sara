{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.Frontmatter.Remap
  ( remapMetadata
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))
import SARA.Monad (SPath)

-- | Industrial metadata remapper.
--   Expects a list of (fromKey, toKey) pairs.
--   Non-fatal: if a key is missing, it is simply skipped (Fix L-06).
remapMetadata :: [(Text, Text)] -> FilePath -> Aeson.Object -> Either (SaraError 'EKFrontmatter) Aeson.Object
remapMetadata rules _pathString obj = Right $ foldr remap obj rules
  where
    remap (from, to) acc = case KM.lookup (K.fromText from) acc of
      Just val -> KM.insert (K.fromText to) val (KM.delete (K.fromText from) acc)
      Nothing -> acc
