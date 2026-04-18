{-# LANGUAGE OverloadedStrings #-}

module SARA.Validator.AssetChecker
  ( checkAssetReferences
  ) where

import SARA.Error (SaraError(..), SourcePos(..), AnySaraError(..))
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative, (</>), takeDirectory)
import qualified Data.HashSet as HS
import SARA.Monad (SPath)

-- | Check if referenced assets (img, script, link) exist in the site graph.
checkAssetReferences
  :: HS.HashSet SPath    -- ^ Site graph (relative paths)
  -> SPath               -- ^ Current source path
  -> SPath               -- ^ Current output path
  -> Text                -- ^ Rendered HTML
  -> [AnySaraError]
checkAssetReferences siteGraph _sourcePath outPath html =
  let tags = parseTags html
      imgSrcs = [ (fromAttrib "src" t, t) | t <- tags, isTagOpenName "img" t ]
      scriptSrcs = [ (fromAttrib "src" t, t) | t <- tags, isTagOpenName "script" t ]
      linkHrefs = [ (fromAttrib "href" t, t) | t <- tags, isTagOpenName "link" t ]
      allRefs = imgSrcs ++ scriptSrcs ++ linkHrefs
  in concatMap (validateAsset siteGraph outPath) allRefs

validateAsset :: HS.HashSet SPath -> SPath -> (Text, Tag Text) -> [AnySaraError]
validateAsset siteGraph outPathText (src, _tag) =
  if T.null src || isExternal src || isDataUrl src
  then []
  else
    let outPath = T.unpack outPathText
        target = T.unpack src
        normalizedTarget = if isRelative target
                           then (takeDirectory outPath </> target)
                           else target
    in if T.pack normalizedTarget `HS.member` siteGraph
       then []
       else [AnySaraError $ ValidatorMissingAsset outPathText (SourcePos outPathText 0 0) src]

isExternal :: Text -> Bool
isExternal l = "http://" `T.isPrefixOf` l || "https://" `T.isPrefixOf` l

isDataUrl :: Text -> Bool
isDataUrl l = "data:" `T.isPrefixOf` l
