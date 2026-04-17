{-# LANGUAGE GADTs #-}

module SARA.Migration.Error
  ( MigrationError(..)
  ) where

import Data.Text (Text)

data MigrationError where
  MigrationUnsupportedShortcode
    :: { migFile :: !FilePath, migShortcode :: !Text }
    -> MigrationError

deriving instance Show MigrationError
