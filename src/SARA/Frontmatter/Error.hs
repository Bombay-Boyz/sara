{-# LANGUAGE GADTs #-}

module SARA.Frontmatter.Error
  ( FrontmatterError(..)
  ) where

import Data.Text (Text)
import SARA.Error (SourcePos)

data FrontmatterError where
  FrontmatterUnknownFormat
    :: { fmfFile :: !FilePath }
    -> FrontmatterError
  FrontmatterParseFailure
    :: { fmfFile :: !FilePath, fmfPos :: !SourcePos, fmfDetail :: !Text }
    -> FrontmatterError
  FrontmatterRemapMissing
    :: { fmfFile :: !FilePath, fmfKey :: !Text }
    -> FrontmatterError

deriving instance Show FrontmatterError
