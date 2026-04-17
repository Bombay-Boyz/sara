{-# LANGUAGE OverloadedStrings #-}

module SARA.Validator.AssetChecker
  ( checkAssetReferences
  ) where

import SARA.Validator.Error
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative)

-- | Checks src attributes for images and scripts.
checkAssetReferences
  :: [FilePath]      -- ^ Valid asset paths
  -> FilePath        -- ^ Current file
  -> Text            -- ^ Rendered HTML
  -> [ValidatorError]
checkAssetReferences validAssets currentFile html =
  let tags = parseTags html
      srcs = [ fromAttrib "src" t | t <- tags, isTagOpenName "img" t || isTagOpenName "script" t ]
  in [ ValidatorMissingAsset currentFile (T.unpack src)
     | src <- srcs
     , not (T.null src)
     , isRelative (T.unpack src)
     , T.unpack src `notElem` validAssets
     ]
