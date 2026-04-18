{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module SARA.Security.PathGuard
  ( ProjectRoot(..)
  , SafePath(..)
  , mkProjectRoot
  , guardPath
  , unSafePath
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import System.FilePath (isAbsolute, makeRelative, normalise, (</>))
import System.Directory (canonicalizePath, makeAbsolute)
import qualified Data.Text as T

-- | Represents a validated root directory.
newtype ProjectRoot = ProjectRoot FilePath deriving (Show, Eq)

-- | Represents a path that has been validated against traversal.
newtype SafePath = SafePath FilePath deriving (Show, Eq)

-- | Unwrap.
unSafePath :: SafePath -> FilePath
unSafePath (SafePath p) = p

-- | Smart constructor for ProjectRoot.
mkProjectRoot :: FilePath -> IO ProjectRoot
mkProjectRoot p = ProjectRoot <$> canonicalizePath p

-- | Industrial path guard: prevents traversal outside root.
guardPath 
  :: ProjectRoot 
  -> FilePath 
  -> Either (SaraError 'EKSecurity) SafePath
guardPath (ProjectRoot root) candidate =
  let normRoot = normalise root
      absCandidate = if isAbsolute candidate then candidate else normRoot </> candidate
      normCandidate = normalise absCandidate
  in if not (T.pack normRoot `T.isPrefixOf` T.pack normCandidate)
     then Left $ SecurityPathTraversal (T.pack "") candidate normRoot
     else let relative = makeRelative normRoot normCandidate
          in if ".." `T.isInfixOf` T.pack relative
          then Left $ SecurityPathTraversal (T.pack "") candidate normRoot
          else Right (SafePath normCandidate)
