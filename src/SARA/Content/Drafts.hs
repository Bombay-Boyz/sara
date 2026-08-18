{-# LANGUAGE OverloadedStrings #-}

-- | Drafts and scheduled (future-dated) posts. Every mainstream SSG
--   treats @draft: true@ and a future @date@ as reasons to exclude a
--   post from a normal build by default — SARA had no equivalent at
--   all before this module, meaning a post typed accidentally with
--   the wrong date, or a work-in-progress draft, had no way to be
--   excluded except deleting it or moving it out of the matched glob.
module SARA.Content.Drafts
  ( isDraft
  , isFutureDated
  , isPublishable
  , filterPublished
  ) where

import SARA.Types (ItemP(..))
import SARA.Monad (SaraM)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (parseTimeM, defaultTimeLocale)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (mapMaybe, listToMaybe)

-- | True iff the item's frontmatter marks it as a draft — @draft:
--   true@ (YAML boolean) or the strings "true"\/"yes" (case-insensitive,
--   since hand-written frontmatter varies and rejecting a plausible
--   spelling silently would be its own small surprise). Absence of
--   the field, or any other value, means "not a draft" — this
--   function is deliberately conservative in the "publish by default"
--   direction, mirroring frontmatter's own general contract with
--   'SARA.Frontmatter.Parser': a field that isn't there doesn't hide
--   content that would otherwise ship.
isDraft :: ItemP v Aeson.Object -> Bool
isDraft item = case KM.lookup (K.fromText "draft") (itemMeta item) of
  Just (Aeson.Bool b)   -> b
  Just (Aeson.String s) -> T.toLower s `elem` ["true", "yes"]
  _                     -> False

-- | True iff the item has a @date@ field that parses and is strictly
--   after 'now'. An unparseable or absent date is treated as "not
--   future" (publish it) rather than silently withheld — a date field
--   SARA can't understand is a data-quality problem for the author to
--   notice from the post actually appearing, not something this
--   function should hide by guessing.
isFutureDated :: UTCTime -> ItemP v Aeson.Object -> Bool
isFutureDated now item = case KM.lookup (K.fromText "date") (itemMeta item) of
  Just (Aeson.String s) -> maybe False (> now) (parseFrontmatterDate (T.unpack s))
  _                     -> False

-- | The two accepted date shapes: a bare date ("2026-01-15") or a full
--   ISO-8601 timestamp. Tried in order; the first that parses wins.
parseFrontmatterDate :: String -> Maybe UTCTime
parseFrontmatterDate s =
  listToMaybe $ mapMaybe (\fmt -> parseTimeM True defaultTimeLocale fmt s)
    [ "%Y-%m-%dT%H:%M:%S%Q%z"
    , "%Y-%m-%dT%H:%M:%S%Q"
    , "%Y-%m-%d %H:%M:%S"
    , "%Y-%m-%d"
    ]

-- | Not a draft, and not scheduled for the future — the two checks a
--   normal build applies together.
isPublishable :: UTCTime -> ItemP v Aeson.Object -> Bool
isPublishable now item = not (isDraft item) && not (isFutureDated now item)

-- | Filters a list of validated items down to the ones a normal build
--   should actually produce output for. Takes the current time via
--   'liftIO' internally rather than asking the caller to supply it,
--   since every call site wants "now" and threading it through by
--   hand would be pure ceremony; callers who need a fixed time for
--   testing should use 'isPublishable' directly.
filterPublished :: [ItemP v Aeson.Object] -> SaraM [ItemP v Aeson.Object]
filterPublished items = do
  now <- liftIO getCurrentTime
  pure $ filter (isPublishable now) items
