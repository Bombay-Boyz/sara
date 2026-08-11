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
  case T.breakOn "}\n" t of
    (f, b) | not (T.null b) -> Right (f <> "}", T.drop 2 b)
    _ -> case T.breakOn "}\r\n" t of
           (f, b) | not (T.null b) -> Right (f <> "}", T.drop 3 b)
           _ -> Right (t, "")
