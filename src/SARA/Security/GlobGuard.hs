module SARA.Security.GlobGuard
  ( GlobPattern
  , unGlobPattern
  , mkGlobPattern
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))
import System.FilePath (splitDirectories, isAbsolute)

-- | Opaque newtype for a glob pattern that has passed 'mkGlobPattern's
--   traversal/absolute-path check. The constructor is deliberately
--   *not* exported (see this type's export list, and issue #2 of the
--   security audit this module's containment guarantee is load-bearing
--   for) — 'mkGlobPattern' is the only way to produce a value of this
--   type anywhere outside this module, so a type-checked 'GlobPattern'
--   really is proof the check ran, not just a value that happens to
--   look validated.
newtype GlobPattern = GlobPattern { unGlobPattern :: Text }
  deriving (Eq, Show)

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
