{-# LANGUAGE OverloadedStrings #-}

module SARA.Validator.LinkChecker
  ( checkInternalLinks
  ) where

import SARA.Error (SaraError(..), SourcePos(..), AnySaraError(..))
import SARA.Monad (SPath)
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative, normalise, (</>), takeDirectory)
import qualified Data.HashSet as HS
import qualified Data.List as L

-- | Internal link checker.
checkInternalLinks
  :: HS.HashSet SPath    -- ^ Valid site paths (site graph)
  -> SPath               -- ^ Current source file
  -> SPath               -- ^ Current output path (relative to _site)
  -> Text                -- ^ Rendered HTML
  -> [AnySaraError]
checkInternalLinks siteGraph _sourcePath outPath html =
  let tags = parseTags html
      links = [ (fromAttrib "href" t, t) | t <- tags, isTagOpenName "a" t ]
  in concatMap (validateLink siteGraph outPath) links

validateLink :: HS.HashSet SPath -> SPath -> (Text, Tag Text) -> [AnySaraError]
validateLink siteGraph outPathText (link, _tag) =
  if T.null link || isExternal link || isAnchor link
  then []
  else
    let outPath = T.unpack outPathText
        target = T.unpack link
        -- If it starts with /, it's absolute from the site root.
        -- We remove the leading / to match our relative-path SiteGraph.
        normalizedTarget = if "/" `L.isPrefixOf` target
                           then normalise (dropWhile (== '/') target)
                           else if isRelative target
                                then normalise (takeDirectory outPath </> target)
                                else normalise target
        targetT = T.pack normalizedTarget
        -- Fix L-03: Try common fallbacks for directory-style links
        possibleTargets = [ targetT
                          , targetT <> "/index.html"
                          , if "/" `T.isSuffixOf` targetT then targetT <> "index.html" else targetT
                          ]
    in if any (`HS.member` siteGraph) possibleTargets
       then []
       else [AnySaraError $ ValidatorBrokenLink outPathText (SourcePos outPathText 0 0) target]

-- | Check if it's an external link.
isExternal :: Text -> Bool
isExternal l = "http://" `T.isPrefixOf` l || "https://" `T.isPrefixOf` l || "mailto:" `T.isPrefixOf` l || "tel:" `T.isPrefixOf` l

-- | Check if it's an anchor.
isAnchor :: Text -> Bool
isAnchor l = "#" `T.isPrefixOf` l
