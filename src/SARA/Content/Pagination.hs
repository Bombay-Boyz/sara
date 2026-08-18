{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}

-- | Pagination for listing pages. Like 'SARA.Content.Taxonomy', built
--   entirely from existing primitives ('SARA.DSL.render',
--   'SARA.Routing.Engine.resolveRoute') — a page of a paginated index
--   is a rendered page, not a new kind of build step.
module SARA.Content.Pagination
  ( paginate
  , buildPaginatedIndex
  ) where

import SARA.Types (ItemP(..), ValidationState(..))
import SARA.Monad (SaraM)
import SARA.DSL (renderSyntheticPage)
import SARA.Content.Summary (itemToSummary)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.FilePath ((</>))

-- | Splits a list into consecutive chunks of at most 'pageSize'
--   items. Total for any 'pageSize', including @<= 0@: rather than
--   looping forever or crashing (an infinite list of empty chunks, or
--   a `div`-by-zero-shaped bug, are both realistic failure modes for
--   a naive recursive chunking function given a non-positive size),
--   a non-positive 'pageSize' is clamped to 1 — one item per page is
--   always a well-defined, terminating answer, and a caller who
--   passes 0 or a negative number by mistake gets a very long but
--   correct result they'll immediately notice, not a hang.
paginate :: Int -> [a] -> [[a]]
paginate pageSize0 = go
  where
    pageSize = max 1 pageSize0
    go [] = []
    go xs = let (page, rest) = splitAt pageSize xs in page : go rest

-- | Renders one page per chunk of 'pageSize' items, to
--   @outDirBase\/index.html@ for page 1 and
--   @outDirBase\/page\/\<n\>\/index.html@ for page n > 1 — the
--   conventional shape (page 1 at the collection's own URL, not
--   @\/page\/1\/@) every mainstream SSG's pager uses, so a
--   hand-written link to the plain listing URL doesn't 404 once
--   pagination is turned on.
--
--   Each page's template context includes @posts@ (that page's items,
--   as 'SARA.Content.Summary.itemToSummary'), @pageNumber@,
--   @totalPages@, and — as plain, always-present fields rather than a
--   Mustache section that may or may not fire — @hasPrev@\/@prevUrl@
--   and @hasNext@\/@nextUrl@, so a template can write
--   @{{#hasNext}}\<a href="{{nextUrl}}"\>Next\<\/a\>{{\/hasNext}}@
--   directly without a separate existence check.
buildPaginatedIndex
  :: FilePath                          -- ^ template
  -> Int                                -- ^ page size
  -> FilePath                           -- ^ output directory base, e.g. "" for site root
  -> [ItemP 'Validated Aeson.Object]
  -> SaraM [ItemP 'Validated Aeson.Object]
buildPaginatedIndex template pageSize outDirBase items = do
  let pages = paginate pageSize items
  let totalPages = length pages
  mapM (uncurry (renderPage template outDirBase totalPages)) (zip [1 ..] pages)

renderPage
  :: FilePath
  -> FilePath
  -> Int
  -> Int
  -> [ItemP 'Validated Aeson.Object]
  -> SaraM (ItemP 'Validated Aeson.Object)
renderPage template outDirBase totalPages pageNumber pageItems =
  renderSyntheticPage template outPath meta (TE.encodeUtf8 (T.pack (show pageNumber)))
  where
    outPath = pageOutputPath outDirBase pageNumber
    hasPrev = pageNumber > 1
    hasNext = pageNumber < totalPages
    meta = KM.fromList
      [ (K.fromText "posts", Aeson.Array (V.fromList (map (itemToSummary 200) pageItems)))
      , (K.fromText "pageNumber", Aeson.Number (fromIntegral pageNumber))
      , (K.fromText "totalPages", Aeson.Number (fromIntegral totalPages))
      , (K.fromText "hasPrev", Aeson.Bool hasPrev)
      , (K.fromText "prevUrl", Aeson.String (T.pack ("/" <> pageOutputPath outDirBase (pageNumber - 1))))
      , (K.fromText "hasNext", Aeson.Bool hasNext)
      , (K.fromText "nextUrl", Aeson.String (T.pack ("/" <> pageOutputPath outDirBase (pageNumber + 1))))
      ]

pageOutputPath :: FilePath -> Int -> FilePath
pageOutputPath outDirBase n
  | n <= 1    = outDirBase </> "index.html"
  | otherwise = outDirBase </> "page" </> show n </> "index.html"
