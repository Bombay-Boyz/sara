module SARA.Security.RegexGuard
  ( SafeRegex
  , mkSafeRegex
  , unSafeRegex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Types (SafeRegex(..))
import SARA.Error (SaraError(..), SaraErrorKind(..))
import Text.Regex.Base.RegexLike (makeRegexOptsM)
import Text.Regex.PCRE.Text (Regex, compBlank, execBlank)
import qualified Data.List as L
import Data.Maybe (isNothing)

-- | Smart constructor for SafeRegex. Total and pure: PCRE compilation
--   is deterministic for a given pattern, so it is checked here via
--   'makeRegexOptsM' specialised to @Maybe@ — the same pure interface
--   'Text.Regex.Base.RegexLike' publishes for this exact purpose —
--   rather than the 'IO'-typed 'Text.Regex.PCRE.Text.compile', which
--   would require an 'unsafePerformIO' this standard bans outright (1.7).
mkSafeRegex :: Text -> Either (SaraError 'EKSecurity) SafeRegex
mkSafeRegex t
  | isNothing (compiledPattern t) = Left $ SecurityRegexReDoS t "Pattern failed to compile"
  | otherwise                     = checkComplexity t

-- | @Nothing@ iff PCRE rejects the pattern at compile time.
compiledPattern :: Text -> Maybe Regex
compiledPattern = makeRegexOptsM compBlank execBlank

-- | Unwrap.
unSafeRegex :: SafeRegex -> Text
unSafeRegex (SafeRegex t) = t

-- | Basic heuristic for ReDoS detection.
checkComplexity :: Text -> Either (SaraError 'EKSecurity) SafeRegex
checkComplexity t =
  let s = T.unpack t
  in if hasNestedQuantifiers s
     then Left $ SecurityRegexReDoS t "Nested quantifiers detected (e.g., (a+)+)"
     else if hasAlternationInRepetition s
     then Left $ SecurityRegexReDoS t "Alternation inside unbounded repetition detected (e.g., (a|ab)+)"
     else if exceedsNestingDepth s 3
     then Left $ SecurityRegexReDoS t "Quantifier nesting depth exceeds limit (3)"
     else Right (SafeRegex t)

-- These are heuristics for ReDoS detection.
-- A full regex parser for safety is out of scope, but we check common patterns.

hasNestedQuantifiers :: String -> Bool
hasNestedQuantifiers s = 
  any (\(prefix, suffix) -> 
            case (safeLast prefix, suffix) of
              (Just ')', c : _) -> isQuantifier c && isQuantifierInLastGroup prefix
              _                 -> False
         ) (allSplits s)
  where
    isQuantifier c = c `elem` ("+*" :: String)

-- | Total variant of 'last': the final element, or 'Nothing' on an
--   empty list. Exists so this module never reaches for the partial
--   'Prelude.last' banned by 1.1\/5.2.
safeLast :: [a] -> Maybe a
safeLast []     = Nothing
safeLast [x]    = Just x
safeLast (_:xs) = safeLast xs

isQuantifierInLastGroup :: String -> Bool
isQuantifierInLastGroup s = 
  case L.elemIndex '(' (reverse s) of
    Just i -> any (`elem` ("+*" :: String)) (take i (reverse s))
    Nothing -> False

allSplits :: [a] -> [([a], [a])]
allSplits xs = [splitAt i xs | i <- [0..length xs]]

hasAlternationInRepetition :: String -> Bool
hasAlternationInRepetition s = "|" `L.isInfixOf` s && ")+" `L.isInfixOf` s

-- | Calculates nesting depth of quantifiers (specifically groups followed by + or *).
exceedsNestingDepth :: String -> Int -> Bool
exceedsNestingDepth s limit = go s 0 0
  where
    go [] _ maxDepth = maxDepth > limit
    go (c:cs) currentDepth maxDepth
      | c == '(' = go cs (currentDepth + 1) maxDepth
      | c == ')' = 
          case cs of
            (next : _) | next == '+' || next == '*' ->
              go cs (currentDepth - 1) (max currentDepth maxDepth)
            _ -> go cs (currentDepth - 1) maxDepth
      | otherwise = go cs currentDepth maxDepth
