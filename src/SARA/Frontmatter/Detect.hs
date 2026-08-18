module SARA.Frontmatter.Detect
  ( FrontmatterFormat(..)
  , detectFormat
  , splitFrontmatter
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import SARA.Error (SaraError(..), SaraErrorKind(..))
import Control.Applicative (asum)
import Data.Maybe (fromMaybe)

data FrontmatterFormat
  = FmYAML
  | FmTOML
  | FmJSON
  | FmNone
  deriving (Eq, Show)

-- | O(1): inspect the first bytes only. Handles both LF and CRLF.
detectFormat :: Text -> FrontmatterFormat
detectFormat t
  | "---" `T.isPrefixOf` t = FmYAML
  | "+++" `T.isPrefixOf` t = FmTOML
  | "{"   `T.isPrefixOf` t = FmJSON
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
      -- Find the closing separator on its own line: try the LF-style
      -- separator, then the CRLF-style one, then the bare fallback, in
      -- that order — the same three-way fallback the original nested
      -- 'case' chain encoded, expressed as one helper tried three
      -- times via 'asum' instead of copy-pasted per separator style.
      (fm, body) = fromMaybe ("", content) $ asum
        [ trySeparator "\n"   sepLF   content
        , trySeparator "\r\n" sepCRLF content
        , trySeparator "\n"   sepBase content
        ]
  in Right (fm, body)
  where
    trySeparator :: Text -> Text -> Text -> Maybe (Text, Text)
    trySeparator lineEnding sep c =
      let needle = lineEnding <> sep
      in case T.breakOn needle c of
           (f, b) | not (T.null b) -> Just (f, T.drop (T.length needle) b)
           _                       -> Nothing

splitJSON :: Text -> Either (SaraError 'EKFrontmatter) (Text, Text)
splitJSON t =
  case T.breakOn "}\n" t of
    (f, b) | not (T.null b) -> Right (f <> "}", T.drop 2 b)
    _ -> case T.breakOn "}\r\n" t of
           (f, b) | not (T.null b) -> Right (f <> "}", T.drop 3 b)
           _ -> Right (t, "")
