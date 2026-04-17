module SARA.Security.RegexGuard
  ( SafeRegex
  , mkSafeRegex
  , unSafeRegex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Types (SafeRegex(..))
import SARA.Error (SaraError(..), SaraErrorKind(..))
import Text.Regex.PCRE.Text (compile, compBlank, execBlank)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.List as L

-- | Smart constructor for SafeRegex.
mkSafeRegex :: Text -> Either (SaraError 'EKSecurity) SafeRegex
mkSafeRegex t = do
  -- 1. Compilation check
  case unsafePerformIO $ compile compBlank execBlank t of
    Left (_, err) -> Left $ SecurityRegexReDoS t (T.pack err)
    Right _ -> do
      -- 2. Structural complexity check (Heuristic)
      checkComplexity t

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
            not (null prefix) && last prefix == ')' 
            && not (null suffix) && isQuantifier (head suffix)
            && isQuantifierInLastGroup prefix
         ) (allSplits s)
  where
    isQuantifier c = c `elem` ("+*" :: String)

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
          if not (null cs) && (head cs == '+' || head cs == '*')
          then go cs (currentDepth - 1) (max (currentDepth) maxDepth)
          else go cs (currentDepth - 1) maxDepth
      | otherwise = go cs currentDepth maxDepth
