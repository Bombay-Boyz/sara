module SARA.Security.RegexGuard
  ( SafeRegex
  , mkSafeRegex
  , unSafeRegex
  , compileSafeRegex
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Types (SafeRegex(..))
import SARA.Error (SaraError(..), SaraErrorKind(..))
import Text.Regex.PCRE.Text (compile, compBlank, execBlank)
import qualified Data.List as L

-- | Smart constructor for SafeRegex.
mkSafeRegex :: Text -> IO (Either (SaraError 'EKSecurity) SafeRegex)
mkSafeRegex t = do
  -- 1. Compilation check
  res <- compile compBlank execBlank t
  case res of
    Left (_, err) -> return $ Left $ SecurityRegexReDoS t (T.pack err)
    Right _ -> return $ 
      -- 2. Structural complexity check (Heuristic)
      checkComplexity t

-- | Compiles and validates a regex.
compileSafeRegex :: Text -> IO (Either (SaraError 'EKSecurity) SafeRegex)
compileSafeRegex = mkSafeRegex

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
hasAlternationInRepetition s = go s (0 :: Int) False
  where
    -- go string currentDepth hasAlternationAtThisDepth
    go :: String -> Int -> Bool -> Bool
    go [] _ _ = False
    go (c:cs) depth hasAlt
      | c == '(' = go cs (depth + 1) False || go cs depth hasAlt
      | c == ')' = 
          let isRepetition = not (null cs) && (head cs == '+' || head cs == '*')
          in (hasAlt && isRepetition) || go cs (depth - 1) False
      | c == '|' = (depth > 0) || go cs depth True
      | otherwise = go cs depth hasAlt

-- | Calculates nesting depth of quantifiers (specifically groups followed by + or *).
exceedsNestingDepth :: String -> Int -> Bool
exceedsNestingDepth s limit = go s (0 :: Int) (0 :: Int)
  where
    go [] _ maxDepth = maxDepth > limit
    go (c:cs) currentDepth maxDepth
      | c == '(' = 
          let isQuantified = isNextQuantified cs (0 :: Int)
          in if isQuantified
             then go cs (currentDepth + 1) (max (currentDepth + 1) maxDepth)
             else go cs currentDepth maxDepth
      | c == ')' = 
          if currentDepth > 0 then go cs (currentDepth - 1) maxDepth else go cs 0 maxDepth
      | otherwise = go cs currentDepth maxDepth

    isNextQuantified :: String -> Int -> Bool
    isNextQuantified [] _ = False
    isNextQuantified (x:xs) d
      | x == '(' = isNextQuantified xs (d + 1)
      | x == ')' = if d == 0 
                   then not (null xs) && (head xs == '+' || head xs == '*')
                   else isNextQuantified xs (d - 1)
      | otherwise = isNextQuantified xs d
