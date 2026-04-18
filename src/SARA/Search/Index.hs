{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

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
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import qualified Data.Text as T
import SARA.Types (Item(..), Route(..))
import Development.Shake
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Char (isAlphaNum, toLower)
import qualified Data.List as L
import Control.Monad (foldM)

data SearchEntry = SearchEntry
  { seUrl     :: !Text
  , seTitle   :: !Text
  , seMeta    :: !Aeson.Object
  } deriving (Show, Generic, Aeson.ToJSON, Aeson.FromJSON)

data InvertedIndex = InvertedIndex
  { documents :: !(Map.Map Int SearchEntry)
  , index     :: !(Map.Map Text (Set.Set Int))
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
--   Industrial Grade: Chunked loading to prevent memory spikes on large sites.
mergePartialIndexes :: [FilePath] -> FilePath -> Action ()
mergePartialIndexes partialPaths outPath = do
  -- Load and merge in chunks of 100 to balance speed and memory
  let chunks = chunkList 100 partialPaths
  (finalDocMap, finalTermMap, _) <- foldM mergeChunk (Map.empty, Map.empty, 0 :: Int) chunks
  let invIdx = InvertedIndex { documents = finalDocMap, index = finalTermMap }
  liftIO $ Aeson.encodeFile outPath invIdx
  where
    chunkList _ [] = []
    chunkList n xs = let (h, t) = splitAt n xs in h : chunkList n t

    mergeChunk (docAcc, termAcc, nextId) paths = do
      partials <- mapM (liftIO . Aeson.decodeFileStrict') paths
      let validPartials = [ p | Just p <- partials ]
          newIds = [nextId ..]
          docMap = Map.fromList $ zip newIds (map piEntry validPartials)
          termMap = L.foldl' addPartial termAcc (zip newIds validPartials)
      return (Map.union docAcc docMap, termMap, nextId + length validPartials)

    addPartial acc (docId, p) =
      L.foldl' (\m token -> Map.insertWith Set.union token (Set.singleton docId) m) acc (piTokens p)

buildInvertedIndex :: [(Int, (Text, Text))] -> Map.Map Text (Set.Set Int)
buildInvertedIndex docs = foldr addDoc Map.empty docs
  where
    addDoc (docId, (title, content)) acc =
      let tokens = tokenize (title <> " " <> content)
      in foldr (\token m -> Map.insertWith Set.union token (Set.singleton docId) m) acc tokens

tokenize :: Text -> [Text]
tokenize = filter (not . T.null) 
         . map (T.filter isAlphaNum . T.map toLower) 
         . T.words

-- | Helper to create an entry from an Item.
mkSearchEntry :: Item v -> (SearchEntry, Text)
mkSearchEntry item = 
  let entry = SearchEntry
        { seUrl     = case itemRoute item of 
                        ResolvedRoute p -> p
                        UnresolvedRoute -> "" -- Should be resolved before indexing
        , seTitle   = case KM.lookup (K.fromText "title") (itemMeta item) of
                        Just (Aeson.String t) -> t
                        _ -> "Untitled"
        , seMeta    = itemMeta item
        }
  in (entry, itemBody item)
