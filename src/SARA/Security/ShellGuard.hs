{-# LANGUAGE DataKinds #-}
module SARA.Security.ShellGuard
  ( safeCmd
  , validateArg
  , validatePath
  , validateCommand
  ) where

import Development.Shake (Action, cmd)
import SARA.Error (SaraError(..), SaraErrorKind(..), renderAnyErrorColor, AnySaraError(..))
import qualified Data.Text as T
import Data.Text (Text)

-- | Execute an external command with arguments.
--   NEVER uses shell string interpolation.
safeCmd
  :: FilePath    -- ^ Executable
  -> [FilePath]  -- ^ Arguments
  -> Action ()
safeCmd exe args = cmd (exe : args)

-- | Validates if a command is allowed to run.
validateCommand :: [Text] -> FilePath -> Action ()
validateCommand allowed exe = 
  let exeT = T.pack exe
  in if exeT `elem` allowed
     then return ()
     else fail $ "SARA SECURITY ERROR: Command '" ++ exe ++ "' is not in the whitelist. "
              ++ "Add it to 'allowedCommands' in sara.yaml to permit execution."

-- | Pre-flight check: reject file paths containing shell metacharacters or NUL bytes.
validateArg :: FilePath -> Either (SaraError 'EKSecurity) ()
validateArg path =
  let forbidden = ";|&><$`\\\"' \t\n\r\0" :: String
      found = filter (`elem` forbidden) path
  in if null found
     then Right ()
     else Left $ SecurityShellInjection path (T.pack $ "Path contains forbidden characters: " ++ found)

-- | Helper for Shake Actions to fail the build if a path is unsafe.
validatePath :: FilePath -> Action ()
validatePath path = case validateArg path of
  Right () -> return ()
  Left err -> fail $ T.unpack (renderAnyErrorColor (AnySaraError err))
