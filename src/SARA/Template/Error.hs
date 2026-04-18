{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module SARA.Template.Error
  ( TemplateError(..)
  ) where

import Data.Text (Text)

data TemplateError where
  TemplateNotFound
    :: { tplName :: !FilePath }
    -> TemplateError
  TemplateCompileError
    :: { tplName :: !FilePath, tplDetail :: !Text }
    -> TemplateError
  TemplateRenderFailure
    :: { tplName :: !FilePath, tplDetail :: !Text }
    -> TemplateError
  TemplateKeyMissing
    :: { tplName :: !FilePath, tplKey :: !Text }
    -> TemplateError
  TemplateUnsafeInterpolation
    :: { tplName :: !FilePath, tplLine :: !Int }
    -> TemplateError

deriving instance Show TemplateError
deriving instance Eq TemplateError
