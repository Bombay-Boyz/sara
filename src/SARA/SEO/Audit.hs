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
import Data.Maybe (isNothing, fromMaybe)
import qualified Data.List as L
import Text.Read (readMaybe)

data AuditResult = AuditPassed | AuditIssues FilePath [AnySaraError]
  deriving (Eq, Show)

-- | Industrial SEO audit for rendered HTML.
auditRenderedHTML :: FilePath -> Text -> AuditResult
auditRenderedHTML pathString html =
  let path = T.pack pathString
      tags = parseTags html
      issues = concat
        [ checkAltAttributes path tags
        , checkHeadingStructure path tags
        , checkTitle path tags
        , checkMetaDescription path tags
        , checkOpenGraph path tags
        , checkUnsafeTags path tags
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
  let headings = [ h | t <- tags, isTagOpen t
                 , let name = getTagName t
                 , "h" `T.isPrefixOf` name
                 , T.length name == 2
                 , Just h <- [readMaybe (T.unpack (T.drop 1 name))]
                 , h >= 1 && h <= 6 ]
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
     else 
       let titleText = innerText $ takeWhile (not . isTagCloseName "title") $ drop 1 $ dropWhile (not . isTagOpenName "title") tags
       in if T.length titleText > 60
          then [AnySaraError (SEOGenericWarning path (SourcePos path 0 0) "Title tag is too long (> 60 chars)")]
          else []

checkMetaDescription :: SPath -> [Tag Text] -> [AnySaraError]
checkMetaDescription path tags =
  let metas = filter (isTagOpenName "meta") tags
      desc = L.find (\t -> getAttrib' "name" t == Just "description") metas
  in case desc of
    Nothing -> [AnySaraError (SEODescriptionMissing path)]
    Just t -> 
      let content = fromMaybe "" (getAttrib' "content" t)
          len = T.length content
      in if len < 50 || len > 160
         then [AnySaraError (SEOGenericWarning path (SourcePos path 0 0) "Meta description should be between 50-160 chars")]
         else []

checkOpenGraph :: SPath -> [Tag Text] -> [AnySaraError]
checkOpenGraph path tags =
  let metas = filter (isTagOpenName "meta") tags
      hasOgTitle = any (\t -> getAttrib' "property" t == Just "og:title") metas
      hasOgImage = any (\t -> getAttrib' "property" t == Just "og:image") metas
      issues = []
      issues1 = if not hasOgTitle then AnySaraError (SEOGenericWarning path (SourcePos path 0 0) "Missing og:title") : issues else issues
      issues2 = if not hasOgImage then AnySaraError (SEOGenericWarning path (SourcePos path 0 0) "Missing og:image") : issues1 else issues1
  in issues2

-- | Helper for TagSoup that handles missing attributes safely.
getAttrib' :: Text -> Tag Text -> Maybe Text
getAttrib' attr (TagOpen _ attrs) = L.lookup attr attrs
getAttrib' _ _ = Nothing

getTagName :: Tag Text -> Text
getTagName (TagOpen name _) = name
getTagName (TagClose name) = name
getTagName _ = ""

checkUnsafeTags :: SPath -> [Tag Text] -> [AnySaraError]
checkUnsafeTags path tags =
  let unsafe = filter (\t -> isTagOpen t && getTagName t `elem` ["script", "iframe", "object", "embed"]) tags
  in [ AnySaraError (SecurityUnsafeTemplate path 0) | _ <- unsafe ]
