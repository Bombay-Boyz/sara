{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.Routing.Engine
  ( resolveRoute
  , detectRouteConflicts
  , regexRoute
  ) where

import SARA.Types (Route(..), RouteState(..))
import SARA.Security.RegexGuard (mkSafeRegex, unSafeRegex)
import SARA.Error (SaraError(..), SaraErrorKind(..))
import System.FilePath ((</>), replaceExtension, splitFileName, dropExtension, splitDirectories)
import Data.List (groupBy, sortOn)
import qualified Data.List as L
import Data.Function (on)
import SARA.Internal.RegexCompat (compileRegexText, matchCaptures)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import qualified Data.Text as T
import qualified Data.Char as Char

type Parser = Parsec Void T.Text

-- | Smart constructor for regex routes.
regexRoute :: T.Text -> T.Text -> Either (SaraError 'EKSecurity) (Route 'Abstract)
regexRoute pat repl = case mkSafeRegex pat of
  Right safe -> Right $ RegexRoute safe repl
  Left err -> Left err

-- | Apply an abstract route to a concrete source path. Total and pure:
--   'RegexRoute' compiles and matches via 'SARA.Internal.RegexCompat's
--   pure @Maybe@-typed interface (itself built on
--   'Text.Regex.Base.RegexLike's 'makeRegexOptsM'\/'matchOnce'), not
--   any 'IO'-typed compile\/execute — because 'RegexRoute'
--   only ever carries a 'SafeRegex', and 'SARA.Security.RegexGuard.mkSafeRegex'
--   already proved that pattern compiles, recompilation here cannot
--   fail for any value this function can actually be called with; the
--   'Nothing' branch below is retained anyway; a proof obligation this
--   function cannot discharge from the type alone still gets a typed,
--   non-partial fallback rather than an unchecked assumption.
resolveRoute
  :: Route 'Abstract
  -> FilePath          -- ^ Source path
  -> Either (SaraError 'EKRouting) (Route 'Resolved)
resolveRoute route_ sourcePath = resolved >>= validatePortable
  where
    resolved = case route_ of
      SlugRoute ->
        Right $ ResolvedRoute (replaceExtension sourcePath "html")
      PrettyRoute ->
        let (dir, file) = splitFileName sourcePath
            name = dropExtension file
        in Right $ ResolvedRoute (dir </> name </> "index.html")
      LiteralRoute path ->
        Right $ ResolvedRoute path
      RegexRoute { rrSafeRegex = safeRegex, rrReplacement = repl } ->
        let pat = unSafeRegex safeRegex
            pathText = T.pack sourcePath
        in case compileRegexText pat of
             Nothing -> Left $ RouteRegexInvalid (T.pack sourcePath) "regex failed to recompile despite passing SafeRegex construction"
             Just compiledRegex ->
                 case matchCaptures compiledRegex pathText of
                  Just spans_ ->
                    let captures = map (\(off, len) -> T.take len (T.drop off pathText))
                                       spans_
                    in case interpolateCaptures captures repl of
                         Right resolvedText -> Right $ ResolvedRoute (T.unpack resolvedText)
                         Left parseErr -> Left $ RouteRegexInvalid repl
                           ("replacement template is malformed: " <> parseErr)
                  Nothing -> 
                    Right $ ResolvedRoute (replaceExtension sourcePath "html")

-- | A resolved path must be writable as a real file on any OS SARA
--   might run on — not just the one that happens to be building it
--   right now. 'SlugRoute'\/'PrettyRoute' derive from a source path
--   that (having been read from disk) is already valid wherever it's
--   currently checked out, but 'RegexRoute' substitutes arbitrary
--   captured text and 'LiteralRoute' takes an arbitrary caller-supplied
--   path — either can produce a path that's fine on Linux/macOS and
--   broken on Windows. Hugo's own docs candidly note it doesn't
--   sanitise this at all; this check exists so SARA doesn't repeat
--   that gap.
validatePortable :: Route 'Resolved -> Either (SaraError 'EKRouting) (Route 'Resolved)
validatePortable r@(ResolvedRoute path)
  | Just reason <- windowsUnsafeReason path = Left $ RouteUnsafeForWindows path reason
  | otherwise                               = Right r

-- | 'Nothing' if every path segment is safe on Windows; otherwise the
--   specific reason, so the error message names the actual problem
--   rather than just "this path is unsafe somewhere."
windowsUnsafeReason :: FilePath -> Maybe T.Text
windowsUnsafeReason path
  | Just c <- L.find (`elem` windowsForbiddenChars) path =
      Just $ "contains character '" <> T.singleton c <> "', which Windows forbids in file names"
  | Just seg <- L.find isWindowsReservedName (splitDirectories path) =
      Just $ "path segment \"" <> T.pack seg <> "\" is a reserved device name on Windows"
  | otherwise = Nothing
  where
    windowsForbiddenChars :: String
    windowsForbiddenChars = "<>:\"|?*"

    isWindowsReservedName :: FilePath -> Bool
    isWindowsReservedName seg =
      let base = map Char.toUpper (dropExtension seg)
      in base `elem`
           ( ["CON", "PRN", "AUX", "NUL"]
           ++ [ "COM" <> show n | n <- [1 .. 9 :: Int] ]
           ++ [ "LPT" <> show n | n <- [1 .. 9 :: Int] ]
           )

-- | Replaces \0, \1, \2... in the replacement string with the
--   corresponding capture group. Total and honest about failure: a
--   malformed replacement template (e.g. a trailing unescaped
--   backslash) is reported via 'Left', not silently returned
--   unparsed-and-uninterpolated the way an earlier version of this
--   function did — a route that looks like it substituted captures but
--   actually didn't is a strictly worse outcome for a user to debug
--   than a clear "your replacement template doesn't parse" error at
--   the point the route was declared.
interpolateCaptures :: [T.Text] -> T.Text -> Either T.Text T.Text
interpolateCaptures caps repl =
  case parse (pInterpolate caps) "" repl of
    Left err   -> Left (T.pack (errorBundlePretty err))
    Right res  -> Right res

-- | Total consumption is required ('<* eof'): without it, 'many'
--   simply stops at the first character it can't match — a trailing,
--   unescaped '\' with nothing after it — and 'parse' reports success
--   with the rest of the input silently dropped, rather than failure.
--   That's a worse silent failure than a reported parse error would
--   be: the caller sees a shorter, wrong-looking replacement with no
--   indication anything was truncated at all. Anchoring to 'eof' turns
--   that into a genuine, reported 'Left' from 'interpolateCaptures'.
pInterpolate :: [T.Text] -> Parser T.Text
pInterpolate caps = T.concat <$> many (pCapture caps <|> pLiteral) <* eof

pCapture :: [T.Text] -> Parser T.Text
pCapture caps = do
  void (chunk "\\")
  digit <- digitChar
  -- 'digitChar' guarantees a character in ['0'..'9'], so 'digitToInt' is
  -- total here in practice; it also skips the 'String'/'read' round trip
  -- that 'read [digit] :: Int' required for a single character.
  let idx = Char.digitToInt digit
  return $ case drop idx caps of
    (val:_) -> val
    []      -> "\\" <> T.singleton digit

pLiteral :: Parser T.Text
pLiteral = (T.singleton <$> anySingleBut '\\') <|> (chunk "\\\\" >> return "\\")

-- | Detect route conflicts in a list of resolved routes.
detectRouteConflicts
  :: [(FilePath, Route 'Resolved)]
  -> [SaraError 'EKRouting]
detectRouteConflicts routes =
  let groups = groupBy ((==) `on` (getResolvedPath . snd)) (sortOn (getResolvedPath . snd) routes)
  in concatMap checkGroup groups
  where
    getResolvedPath :: Route 'Resolved -> FilePath
    getResolvedPath (ResolvedRoute p) = p
    
    checkGroup :: [(FilePath, Route 'Resolved)] -> [SaraError 'EKRouting]
    checkGroup [] = []
    checkGroup [_] = []
    checkGroup ((f1, r1) : x : xs) =
      -- At least two elements are in hand by construction (this clause
      -- only matches lists of length >= 2), so the final element always
      -- exists; finding it via a total left fold over the known-present
      -- tail avoids the partial 'Data.List.last' banned by 1.1/5.2,
      -- rather than relying on the pattern-match guarantee remaining
      -- true if this function is ever refactored.
      let (f2, _) = L.foldl' (\_ later -> later) x xs
      in [RouteConflict f1 f2 (getResolvedPath r1)]
