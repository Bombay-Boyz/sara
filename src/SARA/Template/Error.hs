{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module SARA.Template.Error
  ( TemplateError(..)
  ) where

import Data.Text (Text)

-- | Legacy marker type for external library compatibility if needed.
--   Most SARA template errors now use SaraError 'EKTemplate.
data TemplateError where
  TemplateGeneralError :: { tplName :: !FilePath, tplDetail :: !Text } -> TemplateError

deriving instance Show TemplateError
deriving instance Eq TemplateError
