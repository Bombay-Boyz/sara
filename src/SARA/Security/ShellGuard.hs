module SARA.Security.ShellGuard
  ( safeCmd
  , validateArg
  ) where

import Development.Shake (Action, cmd)
import Control.Monad (forM_)
import SARA.Error (SaraError(..), SaraErrorKind(..), SaraBuildException(..), renderErrorColor)
import Control.Exception (throwIO)
import Development.Shake (liftIO)

-- | Execute an external command with arguments.
--   NEVER uses shell string interpolation.
--
--   Every argument is passed through 'validateArg' before the process
--   is spawned — folded into this one choke point rather than left as
--   a check call sites must remember to make themselves (audit issue
--   #9: 'validateArg' existed, exported, and uncalled by every actual
--   shell-out site in the codebase). This makes it structurally
--   impossible to reach 'cmd' with an unvalidated argument through
--   this module's own public API, the same "the guard runs whether or
--   not the caller remembers to ask for it" property 'SafePath'\/
--   'SafeRegex'\/'GlobPattern' now have after issue #2's fix — without
--   needing a fourth opaque wrapper type, since most arguments here
--   (quality numbers, format flags) aren't paths at all, and NUL
--   bytes are meaningless to reject in something that was never a
--   path to begin with; validating uniformly here costs nothing for
--   those and closes the gap for the ones that are.
safeCmd
  :: FilePath    -- ^ Executable
  -> [FilePath]  -- ^ Arguments
  -> Action ()
safeCmd exe args = do
  forM_ args $ \arg -> case validateArg arg of
    Left err -> liftIO $ throwIO (SaraBuildException (renderErrorColor err))
    Right () -> pure ()
  cmd (exe : args)

-- | Pre-flight check: reject file paths containing NUL bytes.
validateArg :: FilePath -> Either (SaraError 'EKSecurity) ()
validateArg path =
  if '\0' `elem` path
  then Left $ SecurityShellInjection path "Path contains NUL byte"
  else Right ()
