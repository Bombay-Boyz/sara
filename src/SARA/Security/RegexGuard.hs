{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
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

-- | Deep heuristic for ReDoS detection.
hasNestedQuantifiers :: String -> Bool
hasNestedQuantifiers s = go s [] False
  where
    go [] _ _ = False
    go (c:cs) stack groupHasQuant
      | c == '(' = 
          let isQuant = isNextQuantified cs 0
          in go cs (isQuant : stack) False
      | c == ')' = 
          case stack of
            (parentIsQuant : rest) -> 
              (groupHasQuant && parentIsQuant) || go cs rest parentIsQuant
            [] -> go cs [] False
      | isQuantifier c = 
          case stack of
            (thisGroupIsQuant : _) -> thisGroupIsQuant || go cs stack True
            [] -> go cs [] True
      | otherwise = go cs stack groupHasQuant

    isQuantifier c = c `elem` ("+*{" :: String)

hasAlternationInRepetition :: String -> Bool
hasAlternationInRepetition s = go s [] False
  where
    go [] _ _ = False
    go (c:cs) stack groupHasAlt
      | c == '(' = 
          let isQuant = isNextQuantified cs 0
          in go cs (isQuant : stack) False
      | c == ')' = 
          case stack of
            (thisGroupIsQuant : rest) -> 
              (groupHasAlt && thisGroupIsQuant) || go cs rest False
            [] -> go cs [] False
      | c == '|' = go cs stack True
      | otherwise = go cs stack groupHasAlt

-- | Calculates nesting depth of quantifiers.
exceedsNestingDepth :: String -> Int -> Bool
exceedsNestingDepth s limit = go s 0 0
  where
    go [] _ maxD = maxD > limit
    go (c:cs) depth maxD
      | c == '(' = 
          if isNextQuantified cs 0
          then go cs (depth + 1) (max (depth + 1) maxD)
          else go cs depth maxD
      | c == ')' = go cs (max 0 (depth - 1)) maxD
      | otherwise = go cs depth maxD

-- | Industrial fix for quantifier detection.
isNextQuantified :: String -> Int -> Bool
isNextQuantified [] _ = False
isNextQuantified (x:xs) d
  | x == '(' = isNextQuantified xs (d + 1)
  | x == ')' = if d == 0 
               then isFollowedByQuantifier xs
               else isNextQuantified xs (d - 1)
  | otherwise = isNextQuantified xs d

isFollowedByQuantifier :: String -> Bool
isFollowedByQuantifier ('+':_) = True
isFollowedByQuantifier ('*':_) = True
isFollowedByQuantifier ('?':_) = True
isFollowedByQuantifier ('{':_) = True
isFollowedByQuantifier _       = False
