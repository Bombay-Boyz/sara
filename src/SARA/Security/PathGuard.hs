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
import System.FilePath (isAbsolute, makeRelative)
import System.Directory (canonicalizePath, makeAbsolute)
import qualified Data.Text as T
import qualified Data.List as L

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

-- | Industrial path guard: prevents traversal outside root using IO to resolve symlinks.
guardPath 
  :: ProjectRoot 
  -> FilePath 
  -> IO (Either (SaraError 'EKSecurity) SafePath)
guardPath (ProjectRoot root) candidate = do
  -- Resolve symlinks and normalise on both sides
  absCandidate <- makeAbsolute candidate >>= canonicalizePath
  let relative = makeRelative root absCandidate
  if ".." `L.isPrefixOf` relative || isAbsolute relative
    then return $ Left $ SecurityPathTraversal (T.pack "") candidate root
    else return $ Right (SafePath absCandidate)
