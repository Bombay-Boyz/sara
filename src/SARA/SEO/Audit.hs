{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.Audit
  ( AuditResult(..)
  , auditRenderedHTML
  , generateAuditReport
  ) where

import SARA.Error (SaraError(..), AnySaraError(..), SourcePos(..), renderAnyErrorColor)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.HTML.TagSoup
import Text.Read (readMaybe)

data AuditResult = AuditPassed | AuditIssues FilePath [AnySaraError]

-- | Audits a single rendered HTML file for common SEO and accessibility issues.
auditRenderedHTML :: FilePath -> Text -> AuditResult
auditRenderedHTML path html = 
  let tags = parseTags html
      issues = concat
        [ checkImgAltTags path tags
        , checkHeadingHierarchy path tags
        , checkTitleTag path tags
        ]
  in if null issues then AuditPassed else AuditIssues path issues

-- | Internal check for missing alt tags on images.
checkImgAltTags :: FilePath -> [Tag Text] -> [AnySaraError]
checkImgAltTags path tags =
  [ AnySaraError (SEOAltMissing path (SourcePos path 0 0) (renderTags [t]))
  | t <- tags
  , isTagOpenName "img" t
  , T.null (fromAttrib "alt" t)
  ]

-- | Internal check for skipped heading levels (e.g., h1 followed by h3).
checkHeadingHierarchy :: FilePath -> [Tag Text] -> [AnySaraError]
checkHeadingHierarchy path tags =
  let headings = [ h | t <- tags
                     , let name = getTagName t
                     , T.length name == 2 
                     , T.head name == 'h' 
                     , Just h <- [readMaybe (T.unpack $ T.drop 1 name) :: Maybe Int]
                     ]
      go [] _ = []
      go (h:hs) prev =
        if h > prev + 1
        then AnySaraError (SEOHeadingSkip path (SourcePos path 0 0) prev h) : go hs h
        else go hs h
  in go headings 0

-- | Internal check for title tag presence and quality.
checkTitleTag :: FilePath -> [Tag Text] -> [AnySaraError]
checkTitleTag path tags =
  -- If we can find ANY text inside ANY title tag pair, we consider it valid.
  let titleText = T.toLower $ renderTags tags
      hasTitleTag = "<title" `T.isInfixOf` titleText && "</title>" `T.isInfixOf` titleText
      -- We'll still use TagSoup for the actual check if possible
      titleSections = sections (\t -> case t of 
                                    TagOpen n _ -> T.toLower n == "title"
                                    _ -> False) tags
      hasValidContent = any (\s -> not (T.null (T.strip (innerText s)))) titleSections
  in if not (hasTitleTag || hasValidContent)
     then [AnySaraError (SEOTitleMissing path)]
     else []

-- | Generates an HTML report summarizing all audit issues.
generateAuditReport :: [AuditResult] -> FilePath -> IO ()
generateAuditReport results outPath = do
  let report = T.unlines $
        [ "<html><head><title>SARA SEO Audit Report</title>"
        , "<style>body{font-family:sans-serif;margin:2em;} .issue{margin-bottom:1em;padding:1em;border:1px solid #ccc;} .file{font-weight:bold;} .error{color:red;}</style>"
        , "</head><body>"
        , "<h1>SARA SEO Industrial Audit</h1>"
        ] ++ map renderResult results ++ ["</body></html>"]
  TIO.writeFile outPath report
  where
    renderResult AuditPassed = ""
    renderResult (AuditIssues path issues) = T.unlines $
      [ "<div class='issue'>"
      , "<div class='file'>File: " <> T.pack path <> "</div>"
      ] ++ map (\e -> "<div class='error'>" <> renderAnyErrorColor e <> "</div>") issues ++ ["</div>"]

getTagName :: Tag Text -> Text
getTagName (TagOpen name _) = name
getTagName _ = ""
