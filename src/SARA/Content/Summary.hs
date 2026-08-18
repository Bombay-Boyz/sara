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
import Data.Maybe (fromMaybe)
import SARA.Internal.Aeson (lookupText)
import Text.HTML.TagSoup (parseTags, innerText)

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

    titleValue = Aeson.String $
      fromMaybe (T.pack (itemPath item)) (lookupText "title" (itemMeta item))

    dateValue = maybe Aeson.Null Aeson.String (lookupText "date" (itemMeta item))

-- | A short, HTML-tag-stripped preview of rendered body content,
--   truncated at a word boundary where possible rather than
--   mid-word, with an ellipsis marking truncation. Uses
--   'Text.HTML.TagSoup' (already a project dependency, already relied
--   on for the same "get the plain text out of rendered HTML" job in
--   'SARA.SEO.Audit') rather than a hand-written angle-bracket
--   toggle — TagSoup's tokenizer correctly treats a '>' inside a
--   quoted attribute value as part of the attribute, not a tag close,
--   which a naive character-by-character scan gets wrong.
plainTextExcerpt :: Int -> Text -> Text
plainTextExcerpt maxLen html =
  let stripped = T.unwords . T.words $ innerText (parseTags html)  -- collapse whitespace/newlines left by stripping block tags
  in if T.length stripped <= maxLen
     then stripped
     else T.stripEnd (fitToWordBoundary (T.take maxLen stripped)) <> "\x2026"
  where
    -- If the truncation point landed mid-word, back up to the last
    -- preceding space so the excerpt doesn't end on a half-word.
    fitToWordBoundary :: Text -> Text
    fitToWordBoundary t = case T.breakOnEnd " " t of
      (before, _after) | not (T.null before) -> T.stripEnd before
      _                                      -> t
