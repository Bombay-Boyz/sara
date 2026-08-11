{-# LANGUAGE OverloadedStrings #-}

module SARA.Validator.LinkChecker
  ( checkInternalLinks
  ) where

import SARA.Error (SaraError(..), SourcePos(..), AnySaraError(..))
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import System.FilePath (isRelative, normalise, (</>), takeDirectory, dropDrive)
import qualified Data.HashSet as HS

-- | Internal link checker.
checkInternalLinks
  :: FilePath            -- ^ Site output directory (e.g. "_site") — the
                          --   same prefix baked into every 'siteGraph' key
  -> HS.HashSet FilePath -- ^ Valid site paths (site graph)
  -> FilePath            -- ^ Current source file
  -> FilePath            -- ^ Current output path (relative to _site)
  -> Text                -- ^ Rendered HTML
  -> [AnySaraError]
checkInternalLinks outDir siteGraph _sourcePath outPath html =
  let tags = parseTags html
      links = [ (fromAttrib "href" t, t) | t <- tags, isTagOpenName "a" t ]
  in concatMap (validateLink outDir siteGraph outPath) links

validateLink :: FilePath -> HS.HashSet FilePath -> FilePath -> (Text, Tag Text) -> [AnySaraError]
validateLink outDir siteGraph outPath (link, _tag) =
  if T.null link || isExternal link || isAnchor link
  then []
  else
    let target = T.unpack link
        -- Every 'siteGraph' key (see 'SARA.Internal.Planner.collectOutputs')
        -- is rooted at 'outDir' (e.g. "_site/posts/x.html"), so any
        -- href this function resolves must land in that same
        -- key-space to be comparable at all — a relative href
        -- resolves against the *current page's own directory*, an
        -- absolute (root-relative, leading '/') href resolves against
        -- the site root, but both still need 'outDir' folded in
        -- afterward. Before this fix, the relative branch resolved to
        -- a bare, outDir-less path (e.g. "posts/x.html") that could
        -- never match a real 'collectOutputs' key — every relative
        -- internal link in a real build was silently unverifiable,
        -- just never exercised by any template that had one until
        -- this session's taxonomy\/pagination work.
        normalizedTarget
          | isRelative target = normalise (outDir </> takeDirectory outPath </> target)
          | otherwise          = normalise (outDir </> dropDrive target)
    in if normalizedTarget `HS.member` siteGraph
       then []
       else [AnySaraError $ ValidatorBrokenLink outPath (SourcePos outPath 0 0) target]

isExternal :: Text -> Bool
isExternal l = "http://" `T.isPrefixOf` l || "https://" `T.isPrefixOf` l || "mailto:" `T.isPrefixOf` l

isAnchor :: Text -> Bool
isAnchor l = "#" `T.isPrefixOf` l
