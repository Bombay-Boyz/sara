{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric #-}

module SARA.Markdown.Error
  ( MarkdownError(..)
  ) where

import Data.Text (Text)
import SARA.Error (SourcePos)
import GHC.Generics (Generic)

data MarkdownError where
  MarkdownParseError
    :: { mfmFile :: !FilePath, mfmPos :: !SourcePos, mfmDetail :: !Text }
    -> MarkdownError
  MarkdownShortcodeInvalid
    :: { mfmFile :: !FilePath, mfmPos :: !SourcePos, mfmName :: !Text }
    -> MarkdownError

deriving instance Show MarkdownError
