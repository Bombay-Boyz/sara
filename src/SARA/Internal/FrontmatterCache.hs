{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | A persistent, on-disk cache of parsed frontmatter, closing the
--   gap engineering roadmap item #3 named: 'SARA.DSL.match' re-globs
--   the filesystem and re-reads *and re-parses* every matched file's
--   frontmatter on every single rebuild — including every file-watch
--   triggered rebuild during @sara serve@ — even for the N-1 files
--   that weren't the one just saved. On a large site this cost is
--   directly proportional to total post count, paid on every save
--   regardless of how many files actually changed.
--
--   __Why this can't just be a Shake oracle__: unlike
--   'SARA.Internal.Hash.addBlake3Oracle' or the template/LQIP
--   oracles, which exist to let *downstream Shake rules* skip
--   re-running when a content hash hasn't changed, oracles are
--   recomputed fresh every single session by Shake's own design —
--   they help avoid unnecessary rule re-*triggering*, not unnecessary
--   oracle re-*computation*. 'SARA.DSL.match' runs entirely outside
--   Shake's rule graph in the first place (it's an ordinary 'IO'
--   computation that runs before any @Rules ()@ structure is even
--   built from its result), so there is no Shake mechanism to hook
--   into here at all — this module is a small, standalone persistent
--   cache for exactly this one purpose instead.
--
--   __Scope, deliberately narrow__: this caches only the parsed
--   frontmatter (an 'Aeson.Object') and raw markdown body text for a
--   file — not the fully rendered HTML 'SARA.Types.Item'. Markdown
--   rendering depends on a caller-supplied shortcode handler closure
--   that isn't safely comparable or cacheable across runs, so it
--   still re-renders every time; what this cache removes is
--   specifically the YAML\/TOML frontmatter-format detection and
--   parsing cost, which is pure, deterministic, and safe to skip
--   whenever the file is provably unchanged.
--
--   __Staleness detection__: keyed on a content hash of the file's
--   bytes (via 'SARA.Internal.Hash.contentHash'), *not* modification
--   time. An earlier version of this module compared modification
--   times instead — cheaper in principle (no need to read the file
--   at all to check freshness) but a genuine correctness bug in
--   practice, caught directly while testing this module against a
--   real, rapid edit-and-rebuild: many filesystems report mtime at
--   only one-second resolution, so a build immediately followed by a
--   quick edit and rebuild — exactly the pattern @sara serve@'s
--   file-watch loop produces on every save — can land within the
--   same granularity window and be silently treated as unchanged,
--   serving stale cached frontmatter for a file that had, in fact,
--   just changed. Content hashing has no such window: it's exact, at
--   the cost of needing the file's bytes, which
--   'SARA.DSL.readMarkdownWith' already reads unconditionally anyway
--   (to compute 'SARA.Types.ItemP''s own @itemHash@ field), so this
--   costs nothing beyond a read this module's caller was already
--   doing.
module SARA.Internal.FrontmatterCache
  ( FrontmatterCache
  , loadFrontmatterCache
  , persistFrontmatterCache
  , lookupFresh
  , recordEntry
  ) where

import Control.Exception (catch, SomeException)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import GHC.Generics (Generic)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), takeDirectory)
import SARA.Security.PathGuard (ProjectRoot(..))
import SARA.Internal.Hash (contentHash)

-- | One cached file's last-seen content hash, parsed frontmatter, and
--   raw markdown body.
data CacheEntry = CacheEntry
  { ceContentHash :: !Text
  , ceMeta        :: !Aeson.Object
  , ceBody        :: !Text
  } deriving (Show, Generic)

instance Aeson.ToJSON CacheEntry where
  toJSON e = Aeson.object
    [ "contentHash" .= ceContentHash e
    , "meta"        .= ceMeta e
    , "body"        .= ceBody e
    ]

instance Aeson.FromJSON CacheEntry where
  parseJSON = Aeson.withObject "CacheEntry" $ \o ->
    CacheEntry <$> o .: "contentHash" <*> o .: "meta" <*> o .: "body"

-- | A live, in-memory, mutable view of the cache for the duration of
--   one build — unlike 'SARA.Monad.SaraEnv's other fields, this
--   genuinely can't be reified as a plain up-front value the way
--   e.g. its output-paths set can, since its final contents depend on
--   which files individually turn out to be unchanged vs. changed,
--   only known incrementally as 'SARA.DSL.match' processes each one
--   in turn. Loaded once at the start of a run via
--   'loadFrontmatterCache' and written back once at the end via
--   'persistFrontmatterCache'.
newtype FrontmatterCache = FrontmatterCache (IORef (Map.Map FilePath CacheEntry))

cacheFilePath :: ProjectRoot -> FilePath
cacheFilePath (ProjectRoot root) = root </> ".sara" </> "frontmatter-cache.json"

-- | Load the on-disk cache, if any. Never fails the build over a
--   missing, corrupt, or unreadable cache file — any problem here
--   just means starting from an empty cache, exactly as if this were
--   the first build ever run against this project.
loadFrontmatterCache :: ProjectRoot -> IO FrontmatterCache
loadFrontmatterCache root = do
  let path = cacheFilePath root
  exists <- doesFileExist path
  entries <-
    if not exists
      then pure Map.empty
      else (fromMaybe' <$> Aeson.decodeFileStrict' path) `catch` \(_ :: SomeException) -> pure Map.empty
  FrontmatterCache <$> newIORef entries
  where
    fromMaybe' = maybe Map.empty id

-- | Write the cache's current contents back to disk, creating
--   @.sara/@ if this is the first time anything has been cached for
--   this project.
persistFrontmatterCache :: ProjectRoot -> FrontmatterCache -> IO ()
persistFrontmatterCache root (FrontmatterCache ref) = do
  let path = cacheFilePath root
  createDirectoryIfMissing True (takeDirectory path)
  entries <- readIORef ref
  Aeson.encodeFile path entries

-- | Look up a file in the cache, given its already-read raw bytes,
--   returning its parsed frontmatter+body iff those bytes hash to the
--   same value cached for this path — i.e. iff it's safe to skip
--   re-parsing.
lookupFresh :: FrontmatterCache -> FilePath -> BS.ByteString -> IO (Maybe (Aeson.Object, Text))
lookupFresh (FrontmatterCache ref) path rawBytes = do
  entries <- readIORef ref
  let currentHash = contentHash rawBytes
  pure $ case Map.lookup path entries of
    Just entry | ceContentHash entry == currentHash -> Just (ceMeta entry, ceBody entry)
    _ -> Nothing

-- | Record a freshly-parsed file's result in the cache (in memory —
--   see 'persistFrontmatterCache' for writing it to disk), keyed on
--   the hash of the same raw bytes that produced this parse.
recordEntry :: FrontmatterCache -> FilePath -> BS.ByteString -> Aeson.Object -> Text -> IO ()
recordEntry (FrontmatterCache ref) path rawBytes meta body =
  modifyIORef' ref (Map.insert path (CacheEntry (contentHash rawBytes) meta body))
