module SARA.Security.PathGuard
  ( ProjectRoot(..)
  , SafePath
  , unSafePath
  , mkProjectRoot
  , guardPath
  ) where

import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, isRelative, (</>))
import qualified Data.List as L
import SARA.Error (SaraError(..), SaraErrorKind(..))

-- | Opaque newtype for the project root.
newtype ProjectRoot = ProjectRoot FilePath
  deriving (Eq, Show)

-- | Opaque newtype for a path confirmed to be within the project
--   root. The constructor is deliberately *not* exported (see this
--   type's export list, and issue #2 of the security audit this
--   module's containment guarantee is load-bearing for) — 'guardPath'
--   is the only way to produce a value of this type anywhere outside
--   this module.
newtype SafePath = SafePath { unSafePath :: FilePath }
  deriving (Eq, Show)

-- | Construct a 'ProjectRoot' from a path.
mkProjectRoot :: FilePath -> IO ProjectRoot
mkProjectRoot path = ProjectRoot <$> canonicalizePath path

-- | Confirm a path is within the project root, resolving symlinks
--   before the containment check so a symlink checked into (or
--   generated inside) the content tree that points outside the
--   project root is caught rather than passing on its pre-resolution
--   structure alone (audit issue #6). 'IO'-returning of necessity —
--   symlink resolution is inherently a filesystem operation — rather
--   than the module's previous pure signature, which could only ever
--   check the candidate's textual shape, not where it actually
--   resolves to on disk.
--
--   Rejects any candidate containing a NUL byte outright, before the
--   traversal/canonicalization/prefix checks run: a NUL byte can be
--   used to defeat string-based validation on systems whose
--   underlying C calls (or a downstream FFI boundary) truncate a path
--   at the first NUL, letting a validated-looking prefix mask a
--   different real target. This check alone stays pure and precedes
--   any filesystem call, so a NUL-containing candidate never reaches
--   'canonicalizePath' at all.
guardPath
  :: ProjectRoot
  -> FilePath               -- ^ Candidate path
  -> IO (Either (SaraError 'EKSecurity) SafePath)
guardPath (ProjectRoot _) candidate
  | '\0' `L.elem` candidate =
      pure $ Left $ SecurityPathInvalidByte candidate "Path contains a NUL byte"
guardPath (ProjectRoot root) candidate
  | null candidate = pure $ Right (SafePath (normalise root))
  | ".." `L.elem` splitDirectories (normalise candidate) =
      -- Structural check on the *un*resolved candidate, ahead of
      -- canonicalization: defense in depth alongside the canonical
      -- check below, not a substitute for it (a traversal segment
      -- here is rejected outright even before we ask the filesystem
      -- what it actually resolves to).
      pure $ Left $ SecurityPathTraversal "" candidate normRoot
  | otherwise = do
      let absCandidate = if isRelative candidate then normRoot </> candidate else candidate
      candCanon <- canonicalizePath absCandidate
      let candSegments = splitDirectories candCanon
          rootSegments  = splitDirectories normRoot
      pure $
        if rootSegments `L.isPrefixOf` candSegments
        then Right (SafePath candCanon)
        else Left $ SecurityPathTraversal "" candidate normRoot
  where
    -- 'mkProjectRoot' already canonicalized the root once, at
    -- startup; 'normalise' here is just tidying, not re-resolving.
    normRoot = normalise root
