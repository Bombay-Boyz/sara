{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module SARA.SEO.Error
  ( SEOError(..)
  ) where

import Data.Text (Text)
import SARA.Error (SourcePos)

data SEOError where
  SEOAltMissing
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoSrc :: !Text }
    -> SEOError
  SEOHeadingSkip
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoFrom :: !Int, seoTo :: !Int }
    -> SEOError
  SEOTitleMissing
    :: { seoFile :: !FilePath }
    -> SEOError
