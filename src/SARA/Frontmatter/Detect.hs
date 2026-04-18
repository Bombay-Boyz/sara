{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module SARA.Frontmatter.Detect
  ( FrontmatterFormat(..)
  , detectFormat
  , splitFrontmatter
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))

data FrontmatterFormat
  = FmYAML
  | FmTOML
  | FmJSON
  | FmNone
  deriving (Eq, Show)

-- | O(1): inspect the first bytes only. Handles both LF and CRLF.
detectFormat :: Text -> FrontmatterFormat
detectFormat t
  | "---\n" `T.isPrefixOf` t || "---\r\n" `T.isPrefixOf` t = FmYAML
  | "+++\n" `T.isPrefixOf` t || "+++\r\n" `T.isPrefixOf` t = FmTOML
  | "{\n"   `T.isPrefixOf` t || "{\r\n"   `T.isPrefixOf` t = FmJSON
  | otherwise = FmNone

-- | Splits the file into (frontmatter, body).
splitFrontmatter
  :: FrontmatterFormat
  -> Text
  -> Either (SaraError 'EKFrontmatter) (Text, Text)
splitFrontmatter fmt t = case fmt of
  FmNone -> Right ("", t)
  FmYAML -> splitBy "---\n" "---\r\n" "---" t
  FmTOML -> splitBy "+++\n" "+++\r\n" "+++" t
  FmJSON -> splitJSON t

splitBy :: Text -> Text -> Text -> Text -> Either (SaraError 'EKFrontmatter) (Text, Text)
splitBy sepLF sepCRLF sepBase t =
  let -- The frontmatter starts AFTER the first separator + its newline
      (firstLine, rest) = T.breakOn "\n" t
      content = if T.strip firstLine == T.strip sepBase 
                then T.drop 1 rest -- Drop the \n itself
                else t
      -- Find the closing separator on its own line
      (fm, body) = case T.breakOn ("\n" <> sepLF) content of
        (f, b) | not (T.null b) -> (f, T.drop (T.length sepLF + 1) b)
        _ -> case T.breakOn ("\r\n" <> sepCRLF) content of
               (f, b) | not (T.null b) -> (f, T.drop (T.length sepCRLF + 2) b)
               _ -> case T.breakOn ("\n" <> sepBase) content of
                      (f, b) | not (T.null b) -> (f, T.drop (T.length sepBase + 1) b)
                      _ -> ("", content)
  in Right (fm, body)

splitJSON :: Text -> Either (SaraError 'EKFrontmatter) (Text, Text)
splitJSON t = 
  let (fm, rest) = findJSONEnd t
  in if T.null rest && not ("}" `T.isSuffixOf` T.strip fm)
     then Right (t, "") 
     else Right (fm, rest)

-- | Find the end of a JSON object by tracking brace depth.
findJSONEnd :: Text -> (Text, Text)
findJSONEnd t = go (T.unpack t) 0 []
  where
    go [] _ acc = (T.pack (reverse acc), "")
    go (c:cs) depth acc
      | c == '{' = go cs (depth + 1) (c:acc)
      | c == '}' = 
          if depth == 1
          then (T.pack (reverse (c:acc)), T.pack cs)
          else go cs (depth - 1) (c:acc)
      | otherwise = go cs depth (c:acc)
