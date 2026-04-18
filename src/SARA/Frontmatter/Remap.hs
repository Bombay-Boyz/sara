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

-- | Industrial metadata remapper.
--   Expects a list of (fromKey, toKey) pairs.
--   Non-fatal: if a key is missing, it returns a warning but continues.
remapMetadata :: [(Text, Text)] -> FilePath -> Aeson.Object -> ([SaraError 'EKFrontmatter], Aeson.Object)
remapMetadata rules pathString obj = foldr remap ([], obj) rules
  where
    remap (from, to) (errs, acc) = case KM.lookup (K.fromText from) acc of
      Just val -> (errs, KM.insert (K.fromText to) val (KM.delete (K.fromText from) acc))
      Nothing  -> (FrontmatterRemapMissing (T.pack pathString) from : errs, acc)
