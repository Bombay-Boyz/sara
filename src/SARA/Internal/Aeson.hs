{-# LANGUAGE OverloadedStrings #-}

-- | Small, shared helpers for reading item metadata ('Data.Aeson.Object')
--   values. Exists because "look up a JSON string field, falling back to
--   a default if it's absent or a different JSON type" was independently
--   reimplemented, identically, in four modules — see 'lookupText'.
module SARA.Internal.Aeson
  ( lookupText
  ) where

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

-- | Look up a metadata field expected to hold a JSON string. 'Nothing'
--   both when the key is absent /and/ when it's present but holds a
--   different JSON type (a number, object, etc.) — callers combine this
--   with 'Data.Maybe.fromMaybe'\/'maybe' to supply their own default,
--   since the right fallback (a literal string, the item's source path,
--   and so on) differs by call site.
lookupText :: Text -> Aeson.Object -> Maybe Text
lookupText key obj = case KM.lookup (K.fromText key) obj of
  Just (Aeson.String t) -> Just t
  _                      -> Nothing
