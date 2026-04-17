{-# LANGUAGE OverloadedStrings #-}

module SARA.Validator.LinkChecker
  ( checkInternalLinks
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..), SourcePos(..), AnySaraError(..))
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative, normalise, (</>), takeDirectory)
import qualified Data.HashSet as HS

-- | Internal link checker.
checkInternalLinks
  :: HS.HashSet FilePath -- ^ Valid site paths (site graph)
  -> FilePath            -- ^ Current source file
  -> FilePath            -- ^ Current output path (relative to _site)
  -> Text                -- ^ Rendered HTML
  -> [AnySaraError]
checkInternalLinks siteGraph _sourcePath outPath html =
  let tags = parseTags html
      links = [ (fromAttrib "href" t, t) | t <- tags, isTagOpenName "a" t ]
  in concatMap (validateLink siteGraph outPath) links

validateLink :: HS.HashSet FilePath -> FilePath -> (Text, Tag Text) -> [AnySaraError]
validateLink siteGraph outPath (link, _tag) =
  if T.null link || isExternal link || isAnchor link
  then []
  else
    let target = T.unpack link
        normalizedTarget = if isRelative target
                           then normalise (takeDirectory outPath </> target)
                           else normalise target -- assume absolute from root
    in if normalizedTarget `HS.member` siteGraph
       then []
       else [AnySaraError $ ValidatorBrokenLink outPath (SourcePos outPath 0 0) target]

isExternal :: Text -> Bool
isExternal l = "http://" `T.isPrefixOf` l || "https://" `T.isPrefixOf` l || "mailto:" `T.isPrefixOf` l

isAnchor :: Text -> Bool
isAnchor l = "#" `T.isPrefixOf` l
