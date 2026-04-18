{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.Audit
  ( auditRenderedHTML
  , AuditResult(..)
  ) where

import SARA.Error (SaraError(..), AnySaraError(..), SourcePos(..))
import SARA.Monad (SPath)
import Text.HTML.TagSoup
import Data.Text (Text)
import qualified Data.Text as T
import Data.Maybe (isNothing)
import qualified Data.List as L

data AuditResult = AuditPassed | AuditIssues FilePath [AnySaraError]

-- | Industrial SEO audit for rendered HTML.
auditRenderedHTML :: FilePath -> Text -> AuditResult
auditRenderedHTML pathString html =
  let path = T.pack pathString
      tags = parseTags html
      issues = concat
        [ checkAltAttributes path tags
        , checkHeadingStructure path tags
        , checkTitle path tags
        ]
  in if null issues then AuditPassed else AuditIssues pathString issues

checkAltAttributes :: SPath -> [Tag Text] -> [AnySaraError]
checkAltAttributes path tags =
  let imgs = filter (isTagOpenName "img") tags
      missingAlt = filter (\t -> isNothing (getAttrib' "alt" t)) imgs
  in [ AnySaraError (SEOAltMissing path (SourcePos path 0 0) (renderTags [t])) 
     | t <- missingAlt ]

checkHeadingStructure :: SPath -> [Tag Text] -> [AnySaraError]
checkHeadingStructure path tags =
  let headings = [ read (T.unpack (T.drop 1 (getTagName t))) :: Int 
                 | t <- tags, isTagOpen t, "h" `T.isPrefixOf` getTagName t, T.length (getTagName t) == 2, T.all (`elem` ("123456" :: String)) (T.drop 1 (getTagName t)) ]
      validate (prev, issues) h =
        if h > prev + 1
        then (h, AnySaraError (SEOHeadingSkip path (SourcePos path 0 0) prev h) : issues)
        else (h, issues)
  in snd $ L.foldl' validate (0, []) headings

checkTitle :: SPath -> [Tag Text] -> [AnySaraError]
checkTitle path tags =
  let titleTags = filter (isTagOpenName "title") tags
  in if null titleTags
     then [AnySaraError (SEOTitleMissing path)]
     else []

-- | Helper for TagSoup that handles missing attributes safely.
getAttrib' :: Text -> Tag Text -> Maybe Text
getAttrib' attr (TagOpen _ attrs) = L.lookup attr attrs
getAttrib' _ _ = Nothing

getTagName :: Tag Text -> Text
getTagName (TagOpen name _) = name
getTagName (TagClose name) = name
getTagName _ = ""
