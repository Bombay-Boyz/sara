{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module SARA.Validator.Error
  ( ValidatorError(..)
  ) where

data ValidatorError where
  ValidatorBrokenLink
    :: { valFile :: !FilePath, valTarget :: !FilePath }
    -> ValidatorError
  ValidatorMissingAsset
    :: { valFile :: !FilePath, valSrc :: !FilePath }
    -> ValidatorError

deriving instance Eq ValidatorError
deriving instance Show ValidatorError
