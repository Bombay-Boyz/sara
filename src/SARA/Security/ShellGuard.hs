module SARA.Security.ShellGuard
  ( safeCmd
  , validateArg
  ) where

import Development.Shake (Action, cmd)
import SARA.Error (SaraError(..), SaraErrorKind(..))

-- | Execute an external command with arguments.
--   NEVER uses shell string interpolation.
safeCmd
  :: FilePath    -- ^ Executable
  -> [FilePath]  -- ^ Arguments
  -> Action ()
safeCmd exe args = cmd (exe : args)

-- | Pre-flight check: reject file paths containing NUL bytes.
validateArg :: FilePath -> Either (SaraError 'EKSecurity) ()
validateArg path =
  if '\0' `elem` path
  then Left $ SecurityShellInjection path "Path contains NUL byte"
  else Right ()
