{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SARA.Internal.Hash
  ( BLAKE3Oracle
  , addBlake3Oracle
  , needBlake3
  , LQIPOracle
  , addLQIPOracle
  , askLQIP
  , contentHash
  , projectCacheKey
  , siteScriptCacheKey
  ) where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Control.Monad (void, forM)
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import SARA.Asset.Placeholder (generateLQIP)
import SARA.Error (SaraBuildException(..))
import Control.Exception (throwIO)
import qualified Data.Text as T
import qualified System.Directory as Dir
import System.FilePath ((</>), takeExtension)
import Data.List (sort, isPrefixOf)

-- | Hex-encoded SHA-256 of a byte string — see 'BLAKE3Oracle's Haddock
--   for why SHA-256 rather than BLAKE3. The single definition every
--   content-hashing call site in this codebase shares, rather than
--   each one inlining its own copy of the same three-function
--   composition (as 'addBlake3Oracle' and 'SARA.DSL' previously did
--   independently of each other).
contentHash :: BS.ByteString -> Text
contentHash = T.decodeUtf8 . Base16.encode . SHA256.hash

-- | Historically named for the BLAKE3-based cache-key oracle this
--   replaced. This is a build-cache content key, not a security
--   boundary (nothing here is verified against an adversary — see
--   'SARA.Markdown.Parser' and 'SARA.Security.*' for the modules that
--   actually carry that responsibility), so SHA-256 — available from
--   this environment's apt archive, unlike the original 'blake3'
--   package, which Hackage-only distribution makes unreachable here
--   (see @sara.cabal@'s dependency-provenance note) — is equally fit
--   for purpose: a fast, collision-resistant digest used purely to
--   detect "this file's content changed since the last build."
newtype BLAKE3Oracle = BLAKE3Oracle FilePath
  deriving (Show, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult BLAKE3Oracle = String

addBlake3Oracle :: Rules ()
addBlake3Oracle = void $ addOracle $ \(BLAKE3Oracle path) ->
  liftIO $ T.unpack . contentHash <$> BS.readFile path

needBlake3 :: [FilePath] -> Action ()
needBlake3 paths = do
  _ <- askOracles (map BLAKE3Oracle paths)
  pure ()

newtype LQIPOracle = LQIPOracle FilePath
  deriving (Show, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult LQIPOracle = Text

addLQIPOracle :: Rules ()
addLQIPOracle = void $ addOracle $ \(LQIPOracle path) -> do
  res <- liftIO $ generateLQIP path
  case res of
    Right b64 -> return b64
    -- Shake's Oracle callback runs in 'Action', which (like any '%>'
    -- rule body) has no 'Either'-shaped failure channel of its own —
    -- see 'SaraBuildException's Haddock for why this throw, not a bare
    -- 'Prelude.error', is the standard's recorded, justified escape
    -- hatch for this specific boundary (5.9).
    Left err -> liftIO $ throwIO (SaraBuildException (T.pack ("LQIP Oracle failed for " ++ path ++ ": " ++ err)))

askLQIP :: FilePath -> Action Text
askLQIP path = askOracle (LQIPOracle path)

-- | Directories skipped when computing a project's cache key: build
--   outputs and tool-managed caches, never hand-authored content that
--   should invalidate the key when it changes. Matched by bare
--   directory name at any depth (so a nested @assets/vendor/.git@
--   from a vendored dependency is skipped too, not just a top-level
--   one), and anything starting with @.@ is skipped as a blanket rule
--   covering editor/tool directories this list doesn't name
--   individually (@.vscode@, @.idea@, ...).
cacheKeySkippedDirs :: [FilePath]
cacheKeySkippedDirs = ["_site", ".shake", "_build", "dist-newstyle", ".sara", "dist"]

-- | A deterministic hash of every building-relevant file under a
--   project root — every hand-authored input that could change build
--   output (content, templates, assets, @site.hs@, config), skipping
--   only build outputs and tool caches (see 'cacheKeySkippedDirs').
--   Suitable for direct use as a CI cache key: identical project
--   state on two different machines (or two different points in a
--   single repo's history) produces the identical key, and any
--   change that could affect the built site changes it.
--
--   Deliberately simple and file-content-based rather than trying to
--   reuse Shake's own internal dependency graph: a CI cache key needs
--   to be computable *before* any build has run at all (that's the
--   whole point — deciding whether a build is even necessary), so it
--   can't depend on state a build would itself produce.
projectCacheKey :: FilePath -> IO Text
projectCacheKey root = hashFilesMatching root (const True)

-- | A deterministic hash of just a project's @.hs@ files (@site.hs@
--   itself, plus any sibling modules it imports from the same
--   directory tree) — the narrower cache key
--   @SARA.Internal.Compile.ensureSiteCompiled@ uses to decide whether
--   a previously compiled @site.hs@ binary is still valid, since only
--   these files can affect whether recompiling would even produce a
--   different binary. Deliberately *not* 'projectCacheKey': that one
--   is sensitive to every markdown file in the project, which would
--   force a recompile on every single content edit — exactly the
--   cost this cache exists to avoid paying on every save.
siteScriptCacheKey :: FilePath -> IO Text
siteScriptCacheKey root = hashFilesMatching root isHaskellSource
  where
    isHaskellSource path = takeExtension path == ".hs"

-- | Shared implementation behind 'projectCacheKey' and
--   'siteScriptCacheKey': hash every file under @root@ matching
--   @keep@ (skipping 'cacheKeySkippedDirs' either way), sorted by
--   path for determinism regardless of directory-listing order.
hashFilesMatching :: FilePath -> (FilePath -> Bool) -> IO Text
hashFilesMatching root keep = do
  relPaths <- sort . filter keep <$> listFilesRecursive root ""
  perFileHashes <- forM relPaths $ \relPath -> do
    contents <- BS.readFile (root </> relPath)
    pure (T.pack relPath <> ":" <> contentHash contents)
  pure $ contentHash (T.encodeUtf8 (T.intercalate "\n" perFileHashes))

-- | List every regular file under @root </> relDir@, recursively, as
--   paths relative to @root@ — skipping 'cacheKeySkippedDirs' and any
--   dot-directory encountered along the way.
listFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursive root relDir = do
  entries <- Dir.listDirectory (root </> relDir)
  fmap concat $ forM entries $ \entry -> do
    let relPath = if null relDir then entry else relDir </> entry
    isDir <- Dir.doesDirectoryExist (root </> relPath)
    if isDir
      then if entry `elem` cacheKeySkippedDirs || "." `isPrefixOf` entry
           then pure []
           else listFilesRecursive root relPath
      else pure [relPath]
