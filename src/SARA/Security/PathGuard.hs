module SARA.Security.PathGuard
  ( ProjectRoot(..)
  , SafePath(..)
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

-- | Opaque newtype for a path confirmed to be within the project root.
newtype SafePath = SafePath { unSafePath :: FilePath }
  deriving (Eq, Show)

-- | Construct a 'ProjectRoot' from a path.
mkProjectRoot :: FilePath -> IO ProjectRoot
mkProjectRoot path = ProjectRoot <$> canonicalizePath path

-- | Purely confirm a path is within the project root using structural check.
--   Rejects any candidate containing a NUL byte outright, before the
--   traversal/prefix checks run: a NUL byte can be used to defeat
--   string-based validation on systems whose underlying C calls (or a
--   downstream FFI boundary) truncate a path at the first NUL, letting a
--   validated-looking prefix mask a different real target.
guardPath
  :: ProjectRoot
  -> FilePath               -- ^ Candidate path
  -> Either (SaraError 'EKSecurity) SafePath
guardPath (ProjectRoot _) candidate
  | '\0' `L.elem` candidate =
      Left $ SecurityPathInvalidByte candidate "Path contains a NUL byte"
guardPath (ProjectRoot root) candidate = 
  if null candidate then Right (SafePath (normalise root)) else
  let normRoot = normalise root
      normCand = if isRelative candidate then normRoot </> candidate else normalise candidate
      candSegments = splitDirectories normCand
      rootSegments = splitDirectories normRoot
  in if ".." `L.elem` splitDirectories (normalise candidate)
     then Left $ SecurityPathTraversal "" candidate normRoot
     else if rootSegments `L.isPrefixOf` candSegments
          then Right (SafePath normCand)
          else Left $ SecurityPathTraversal "" candidate normRoot
