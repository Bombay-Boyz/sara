{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

-- | Projects a full 'Item' down to the small summary shape every
--   listing template (index pages, taxonomy pages, pagination pages)
--   actually needs: title, url, date, excerpt. Factored out once,
--   here, rather than duplicated in 'SARA.Content.Taxonomy' and
--   'SARA.Content.Pagination' separately (0.6 of the Haskell
--   Engineering Standard) — a listing page is a listing page whether
--   it's grouped by tag, paginated, or the site's front page.
module SARA.Content.Summary
  ( itemToSummary
  , plainTextExcerpt
  ) where

import SARA.Types (ItemP(..), Route(..), ValidationState(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T

-- | @{ "title": ..., "url": ..., "date": ..., "excerpt": ... }@ — the
--   exact shape 'templates/index.html' already expects under
--   @{{#posts}}@. @title@\/@date@ come from the item's own frontmatter
--   when present (falling back to something reasonable rather than an
--   empty string, since a listing page with a blank title anywhere is
--   worse than one with a visibly-derived placeholder); @url@ is
--   derived from the item's resolved route, never trusted verbatim
--   from user content; @excerpt@ is a short, plain-text preview of the
--   rendered body.
itemToSummary :: Int -> ItemP 'Validated Aeson.Object -> Aeson.Value
itemToSummary excerptLength item =
  Aeson.Object $ KM.fromList
    [ (K.fromText "title", titleValue)
    , (K.fromText "url", Aeson.String (T.pack ("/" <> outputPath)))
    , (K.fromText "date", dateValue)
    , (K.fromText "excerpt", Aeson.String (plainTextExcerpt excerptLength (itemBody item)))
    ]
  where
    outputPath = case itemRoute item of
      ResolvedRoute p -> p

    titleValue = case KM.lookup (K.fromText "title") (itemMeta item) of
      Just v@(Aeson.String _) -> v
      _                       -> Aeson.String (T.pack (itemPath item))

    dateValue = case KM.lookup (K.fromText "date") (itemMeta item) of
      Just v@(Aeson.String _) -> v
      _                       -> Aeson.Null

-- | A short, HTML-tag-stripped preview of rendered body content,
--   truncated at a word boundary where possible rather than
--   mid-word, with an ellipsis marking truncation. Deliberately
--   simple (no full HTML parser): this only needs to produce a
--   reasonable-looking preview, not a faithful re-render, and a naive
--   angle-bracket strip is total and fast for that purpose.
plainTextExcerpt :: Int -> Text -> Text
plainTextExcerpt maxLen html =
  let stripped = T.unwords . T.words $ stripTags html  -- collapse whitespace/newlines left by stripping block tags
  in if T.length stripped <= maxLen
     then stripped
     else T.stripEnd (fitToWordBoundary (T.take maxLen stripped)) <> "\x2026"
  where
    stripTags :: Text -> Text
    stripTags = go False
      where
        go _        ""       = ""
        go False    t = case T.uncons t of
          Just ('<', rest) -> go True rest
          Just (c, rest)   -> T.cons c (go False rest)
          Nothing          -> ""
        go True     t = case T.uncons t of
          Just ('>', rest) -> go False rest
          Just (_, rest)   -> go True rest
          Nothing          -> ""

    -- If the truncation point landed mid-word, back up to the last
    -- preceding space so the excerpt doesn't end on a half-word.
    fitToWordBoundary :: Text -> Text
    fitToWordBoundary t = case T.breakOnEnd " " t of
      (before, _after) | not (T.null before) -> T.stripEnd before
      _                                      -> t
