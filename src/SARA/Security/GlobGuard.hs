{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module SARA.Security.GlobGuard
  ( GlobPattern
  , mkGlobPattern
  , unGlobPattern
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Types (GlobPattern(..))
import SARA.Error (SaraError(..), SaraErrorKind(..))
import System.FilePath (splitDirectories, isAbsolute)

-- | Smart constructor for GlobPattern.
--   Rejects patterns containing '..' or absolute paths.
mkGlobPattern
  :: Text
  -> Either (SaraError 'EKSecurity) GlobPattern
mkGlobPattern t =
  let s = T.unpack t
  in if ".." `elem` splitDirectories s
        || isAbsolute s
     then Left $ SecurityGlobEscape t "Path escapes project root or is absolute"
     else Right $ GlobPattern t
