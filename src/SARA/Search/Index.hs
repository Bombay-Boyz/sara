{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module SARA.Search.Index
  ( SearchEntry(..)
  , InvertedIndex(..)
  , PartialIndex(..)
  , generateSearchIndex
  , generatePartialIndex
  , mergePartialIndexes
  , mkSearchEntry
  ) where

import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (fromMaybe, mapMaybe)
import SARA.Types (Item, ItemP(..), Route(..))
import SARA.Internal.Aeson (lookupText)
import SARA.Search.Stemmer (stem)
import Development.Shake
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAlphaNum, toLower)
import Text.HTML.TagSoup (parseTags, maybeTagText)

-- | A search index's public, purpose-built summary of an item — not
--   its raw frontmatter. This is the same "small, purpose-built
--   summary shape" principle 'SARA.Content.Summary.itemToSummary'
--   already applies to listing pages, applied here too (audit issue
--   #11): the prior shape carried the item's *entire* unfiltered
--   'itemMeta' object into a file the scaffolded 'search.js' fetches
--   publicly, exposing whatever a content author happened to put in
--   frontmatter — internal notes, an editor's email, anything never
--   intended for public display — regardless of whether any template
--   ever rendered it. Only the two fields the shipped search UI
--   actually displays are carried here; a site that wants more can
--   still build its own, since 'mkSearchEntry' remains just one way
--   to populate a 'SearchEntry'.
data SearchEntry = SearchEntry
  { seUrl     :: !Text
  , seTitle   :: !Text
  } deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | @index@ maps each stemmed term to the set of documents containing
--   it, paired with that term's *frequency within each document*
--   (not just presence/absence) — the raw ingredient a client needs
--   to rank results by relevance (e.g. TF-IDF: term frequency here,
--   combined at query time with inverse document frequency derived
--   from @Map.size documents@ and how many documents map to a given
--   term). Scoring itself is deliberately left to the query side
--   rather than baked into a single precomputed number stored here,
--   the same way a real inverted index (Lucene, etc.) separates
--   "what the index records" from "how a query scores it" — so a
--   future ranking change doesn't require re-deriving the index
--   format itself.
data InvertedIndex = InvertedIndex
  { documents :: !(Map.Map Int SearchEntry)
  , index     :: !(Map.Map Text (Map.Map Int Int))
  } deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data PartialIndex = PartialIndex
  { piEntry   :: !SearchEntry
  , piTokens  :: ![Text]
  } deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

-- | Generates a final search index from raw content (Legacy/Simple).
generateSearchIndex
  :: [SearchEntry]
  -> [Text] 
  -> FilePath
  -> Action ()
generateSearchIndex entries contents path = do
  let docMap = Map.fromList $ zip [0..] entries
      termMap = buildInvertedIndex $ zip [0..] (zip (map seTitle entries) contents)
      invIdx = InvertedIndex { documents = docMap, index = termMap }
  liftIO $ Aeson.encodeFile path invIdx

-- | Generates a partial index for a single page.
generatePartialIndex :: SearchEntry -> Text -> FilePath -> Action ()
generatePartialIndex entry content path = do
  let tokens = tokenize (seTitle entry <> " " <> content)
      partial = PartialIndex entry tokens
  liftIO $ Aeson.encodeFile path partial

-- | Merges multiple partial indexes into a single final index.
mergePartialIndexes :: [FilePath] -> FilePath -> Action ()
mergePartialIndexes partialPaths outPath = do
  partials <- mapM (liftIO . Aeson.decodeFileStrict') partialPaths
  let validPartials = [ p | Just p <- partials ]
      docMap = Map.fromList $ zip [0..] (map piEntry validPartials)
      termMap = foldr addPartial Map.empty (zip [0..] validPartials)
      invIdx = InvertedIndex { documents = docMap, index = termMap }
  liftIO $ Aeson.encodeFile outPath invIdx
  where
    addPartial (docId, p) acc = addTermFrequencies docId (piTokens p) acc

buildInvertedIndex :: [(Int, (Text, Text))] -> Map.Map Text (Map.Map Int Int)
buildInvertedIndex docs = foldr addDoc Map.empty docs
  where
    addDoc (docId, (title, content)) acc =
      addTermFrequencies docId (tokenize (title <> " " <> content)) acc

-- | Fold a document's token list into the shared term -> (docId ->
--   frequency) map, counting repeats within this one document rather
--   than collapsing them into presence/absence — see 'InvertedIndex'.
addTermFrequencies :: Int -> [Text] -> Map.Map Text (Map.Map Int Int) -> Map.Map Text (Map.Map Int Int)
addTermFrequencies docId tokens acc =
  foldr bump acc tokens
  where
    bump token = Map.insertWith (Map.unionWith (+)) token (Map.singleton docId 1)

-- | Tokenizes rendered HTML into stemmed, stopword-filtered search
--   terms.
--
--   __Stages, in order__:
--
--   1. Strip HTML tags, keeping only text-node content (via
--      'Text.HTML.TagSoup'). Previously this tokenized the raw HTML
--      *string* directly — a shortcode's own markup (e.g. an
--      @\<img\>@\/@\<picture\>@ tag's attributes) leaked into the
--      index as garbled tokens like @"pimage"@ (@\<p\>{{image@ mashed
--      together), confirmed directly while testing the shortcode
--      escaping fix elsewhere in this codebase. Text nodes are joined
--      with spaces (not 'Text.HTML.TagSoup.innerText', which
--      concatenates adjacent text nodes with no separator at all,
--      merging words across a paragraph boundary like
--      @"...here.Next paragraph..."@) so tag boundaries don't fuse
--      unrelated words together.
--   2. Lower-case and strip non-alphanumeric characters per word,
--      same as before.
--   3. Drop stopwords — semantically empty for search relevance, and
--      otherwise the single most frequent (hence least useful)
--      entries in the index.
--   4. Stem what's left ('SARA.Search.Stemmer.stem'), so
--      "connect"\/"connected"\/"connecting"\/"connection" conflate to
--      one index term instead of four unrelated ones — the entire
--      point of running a stemmer ahead of an inverted index.
tokenize :: Text -> [Text]
tokenize = map stem
         . filter (`Set.notMember` stopwords)
         . filter (not . T.null)
         . map (T.filter isAlphaNum . T.map toLower)
         . T.words
         . stripHtmlToPlainText

stripHtmlToPlainText :: Text -> Text
stripHtmlToPlainText = T.unwords . mapMaybe maybeTagText . parseTags

-- | A small, standard set of high-frequency English words carrying
--   essentially no search-relevance signal on their own. Deliberately
--   conservative (not the full ~180-word NLTK list) — the goal is
--   removing words so common they'd otherwise dominate every
--   document's token list, not attempting a linguistically complete
--   stopword set.
stopwords :: Set.Set Text
stopwords = Set.fromList
  [ "a", "an", "and", "are", "as", "at", "be", "but", "by"
  , "for", "if", "in", "into", "is", "it", "no", "not", "of"
  , "on", "or", "such", "that", "the", "their", "then", "there"
  , "these", "they", "this", "to", "was", "will", "with", "from"
  , "has", "have", "had", "been", "do", "does", "did", "can"
  , "could", "would", "should", "i", "you", "he", "she", "we"
  ]

-- | Helper to create an entry from an Item.
mkSearchEntry :: Item v -> (SearchEntry, Text)
mkSearchEntry item = 
  let rawUrl = case itemRoute item of ResolvedRoute p -> T.pack p
      -- Root-relative, matching 'SARA.Content.Summary.itemToSummary's
      -- own convention for the same reason: the generated
      -- 'search.js' navigates via 'location.href = res.seUrl'
      -- directly, from whatever page the search box was opened on.
      -- A URL with no leading slash resolves relative to the
      -- *current* page rather than the site root — invisible while
      -- testing search from the site's own front page, and wrong on
      -- every other page (e.g. a tag page under /tags/x/).
      url = if "/" `T.isPrefixOf` rawUrl then rawUrl else "/" <> rawUrl
      entry = SearchEntry
        { seUrl     = url
        , seTitle   = fromMaybe "Untitled" (lookupText "title" (itemMeta item))
        }
  in (entry, itemBody item)
