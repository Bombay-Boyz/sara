module SARA.Frontmatter.Remap
  ( RemapSpec
  , remapMetadata
  ) where

import Control.Monad (foldM)
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import SARA.Error (SaraError(..), SaraErrorKind(..))

type RemapSpec = [(Text, Text)]

-- | Renames keys. If 'fromKey' is absent, returns FrontmatterRemapMissing.
--   If 'toKey' already exists, the existing value is preserved (no overwrite).
remapMetadata
  :: RemapSpec
  -> FilePath
  -> Aeson.Object
  -> Either (SaraError 'EKFrontmatter) Aeson.Object
remapMetadata spec path obj = foldM (remapOne path) obj spec

remapOne :: FilePath -> Aeson.Object -> (Text, Text) -> Either (SaraError 'EKFrontmatter) Aeson.Object
remapOne path obj (from, to) =
  let fromKey = K.fromText from
      toKey = K.fromText to
  in case KM.lookup fromKey obj of
    Nothing -> Left $ FrontmatterRemapMissing path from
    Just val ->
      -- If toKey already exists, preserve it but remove fromKey
      if KM.member toKey obj
      then Right $ KM.delete fromKey obj
      else Right $ KM.insert toKey val (KM.delete fromKey obj)
