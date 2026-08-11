{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

module SARA.Diagnostics
  ( Diagnostic(..)
  , QualitySeal(..)
  , renderDiagnostic
  , renderErrorWithContext
  , renderQualitySeal
  ) where

import Prettyprinter
import Prettyprinter.Render.Terminal
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson

data Diagnostic
  = Info Text
  | Warn Text
  | Fail Text

data QualitySeal = QualitySeal
  { qsSecurity    :: !Bool
  , qsSEO         :: !Bool
  , qsPerformance :: !Int -- 0-100 score
  , qsItemCount   :: !Int
  } deriving (Show, Generic, Aeson.ToJSON)

renderDiagnostic :: Diagnostic -> IO ()
renderDiagnostic d = putDoc (format d <> hardline)
  where
    format (Info t) = annotate (color Cyan) (pretty ("i" :: Text)) <+> pretty t
    format (Warn t) = annotate (color Yellow) (pretty ("!" :: Text)) <+> pretty t
    format (Fail t) = annotate (color Red) (pretty ("X" :: Text)) <+> pretty t

renderErrorWithContext :: FilePath -> Int -> Text -> Text -> IO ()
renderErrorWithContext file lineNum snippet suggestion = do
  putDoc $
    annotate (color Red) (pretty ("ERROR" :: Text)) <+> pretty file <> colon <+> pretty lineNum <> hardline <>
    indent 2 (annotate (color Blue) (pretty ("|" :: Text)) <+> pretty snippet) <> hardline <>
    indent 2 (annotate (color Green) (pretty ("-> " :: Text)) <+> pretty suggestion) <> hardline

renderQualitySeal :: QualitySeal -> IO ()
renderQualitySeal qs = do
  putDoc $ hardline <>
    annotate (color Cyan) (pretty ("┌" <> T.replicate 38 "─" <> "┐" :: Text)) <> hardline <>
    annotate (color Cyan) (pretty ("│" :: Text)) <+> annotate (bold <> color White) (pretty (" SARA INDUSTRIAL QUALITY REPORT " :: Text)) <+> annotate (color Cyan) (pretty ("│" :: Text)) <> hardline <>
    annotate (color Cyan) (pretty ("├" <> T.replicate 38 "─" <> "┤" :: Text)) <> hardline <>
    renderLine "Security" (if qsSecurity qs then "IRONCLAD" else "VULNERABLE") (if qsSecurity qs then Green else Red) <>
    renderLine "SEO Audit" (if qsSEO qs then "PASS" else "ISSUES") (if qsSEO qs then Green else Yellow) <>
    renderLine "Performance" (T.pack (show (qsPerformance qs)) <> "/100") (scoreColor (qsPerformance qs)) <>
    renderLine "Pages" (T.pack (show (qsItemCount qs))) Cyan <>
    annotate (color Cyan) (pretty ("└" <> T.replicate 38 "─" <> "┘" :: Text)) <> hardline
  where
    renderLine label val clr =
      let padding = 20 - T.length label
      in annotate (color Cyan) (pretty ("│" :: Text)) <+> pretty label <> pretty (T.replicate padding " ") <> 
         annotate (bold <> color clr) (pretty val) <> 
         pretty (T.replicate (18 - T.length val) " ") <+> annotate (color Cyan) (pretty ("│" :: Text)) <> hardline
    
    scoreColor s | s >= 90   = Green
                 | s >= 50   = Yellow
                 | otherwise = Red
