module SARA.Security.PathGuard
  ( ProjectRoot(..)
  , SafePath(..)
  , mkProjectRoot
  , guardPath
  , guardPathUnsafe
  ) where

import System.Directory (canonicalizePath)
import System.FilePath (normalise, splitDirectories, isRelative, (</>))
import qualified Data.List as L
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..), renderError)

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
guardPath
  :: ProjectRoot
  -> FilePath               -- ^ Candidate path
  -> Either (SaraError 'EKSecurity) SafePath
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

-- | Internal use only; panics if called on an unsafe path.
guardPathUnsafe :: ProjectRoot -> FilePath -> SafePath
guardPathUnsafe root path = case guardPath root path of
  Right p -> p
  Left  e -> error $ "guardPathUnsafe: " ++ T.unpack (renderError e)
