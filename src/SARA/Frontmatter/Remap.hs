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
remapMetadata :: [(Text, Text)] -> FilePath -> Aeson.Object -> Either (SaraError 'EKFrontmatter) Aeson.Object
remapMetadata rules pathString obj = foldr remap (Right obj) rules
  where
    path = T.pack pathString
    remap (from, to) (Right acc) = case KM.lookup (K.fromText from) acc of
      Just val -> Right $ KM.insert (K.fromText to) val (KM.delete (K.fromText from) acc)
      Nothing -> Left $ FrontmatterRemapMissing path from
    remap _ (Left err) = Left err
