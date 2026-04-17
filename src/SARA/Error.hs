{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SARA.Error
  ( SaraError(..)
  , SaraErrorKind(..)
  , SourcePos(..)
  , AuditLevel(..)
  , AnySaraError(..)
  , renderError       -- :: SaraError k -> Text
  , renderErrorColor  -- :: SaraError k -> Text  (ANSI coloured)
  , renderAnyErrorColor
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Prettyprinter
import Prettyprinter.Render.Terminal

-- | Source location carried by every error that originates in user content.
data SourcePos = SourcePos
  { spFile   :: !FilePath
  , spLine   :: !Int
  , spColumn :: !Int
  } deriving (Eq, Show)

-- | Discriminates how the build should respond to an error.
data AuditLevel
  = AuditFail    -- ^ Halt the build immediately.
  | AuditWarn    -- ^ Emit a warning; continue the build.
  deriving (Eq, Ord, Show)

-- | The unified error kind tag, used as the GADT index.
data SaraErrorKind
  = EKFrontmatter
  | EKRouting
  | EKMarkdown
  | EKTemplate
  | EKSEO
  | EKValidator
  | EKAsset
  | EKMigration
  | EKConfig
  | EKSecurity

-- | GADT: each constructor carries the minimum evidence needed
--   to render a precise, human-readable diagnostic.
data SaraError (k :: SaraErrorKind) where

  -- Frontmatter errors
  FrontmatterUnknownFormat
    :: { fmFile :: !FilePath }
    -> SaraError 'EKFrontmatter
  FrontmatterParseFailure
    :: { fmFile :: !FilePath, fmPos :: !SourcePos, fmDetail :: !Text }
    -> SaraError 'EKFrontmatter
  FrontmatterRemapMissing
    :: { fmFile :: !FilePath, fmFrom :: !Text }
    -> SaraError 'EKFrontmatter

  -- Routing errors
  RouteRegexInvalid
    :: { rtPattern :: !Text, rtDetail :: !Text }
    -> SaraError 'EKRouting
  RouteConflict
    :: { rtFile1 :: !FilePath, rtFile2 :: !FilePath, rtOutput :: !FilePath }
    -> SaraError 'EKRouting

  -- Markdown errors
  MarkdownUnsupportedExtension
    :: { mdFile :: !FilePath, mdPos :: !SourcePos, mdFeature :: !Text }
    -> SaraError 'EKMarkdown

  -- Template errors
  TemplateNotFound
    :: { tplName :: !FilePath }
    -> SaraError 'EKTemplate
  TemplateRenderFailure
    :: { tplName :: !FilePath, tplDetail :: !Text }
    -> SaraError 'EKTemplate
  TemplateKeyMissing
    :: { tplName :: !FilePath, tplKey :: !Text }
    -> SaraError 'EKTemplate
  TemplateUnsafeInterpolation
    :: { tplName :: !FilePath, tplLine :: !Int }
    -> SaraError 'EKTemplate

  -- SEO errors
  SEOAltMissing
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoSrc :: !Text }
    -> SaraError 'EKSEO
  SEOHeadingSkip
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoFrom :: !Int, seoTo :: !Int }
    -> SaraError 'EKSEO
  SEOTitleMissing
    :: { seoFile :: !FilePath }
    -> SaraError 'EKSEO
  SEODescriptionMissing
    :: { seoFile :: !FilePath }
    -> SaraError 'EKSEO

  -- Validator errors
  ValidatorBrokenLink
    :: { valFile :: !FilePath, valPos :: !SourcePos, valTarget :: !FilePath }
    -> SaraError 'EKValidator
  ValidatorMissingAsset
    :: { valFile :: !FilePath, valPos :: !SourcePos, valSrc :: !Text }
    -> SaraError 'EKValidator

  -- Asset errors
  AssetProcessingFailed
    :: { astFile :: !FilePath, astDetail :: !Text }
    -> SaraError 'EKAsset

  -- Migration errors
  MigrationUnsupportedShortcode
    :: { migFile :: !FilePath, migShortcode :: !Text }
    -> SaraError 'EKMigration

  -- Config errors
  ConfigKeyMissing
    :: { cfgKey :: !Text }
    -> SaraError 'EKConfig

  -- Security errors
  SecurityPathTraversal
    :: { secFile :: !FilePath, secAttempted :: !FilePath, secRoot :: !FilePath }
    -> SaraError 'EKSecurity
  SecurityGlobEscape
    :: { secGlob :: !Text, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityRegexReDoS
    :: { secPattern :: !Text, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityShellInjection
    :: { secPath :: !FilePath, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityUnsafeTemplate
    :: { secTemplate :: !FilePath, secLine :: !Int }
    -> SaraError 'EKSecurity

-- | Existential wrapper so errors from all subsystems can be collected.
data AnySaraError where
  AnySaraError :: SaraError k -> AnySaraError

deriving instance Show AnySaraError

deriving instance Show (SaraError k)
deriving instance Eq (SaraError k)

-- | Basic renderer (non-colored for now)
renderError :: SaraError k -> Text
renderError e = 
  let doc = prettyError e
  in renderStrict (layoutPretty defaultLayoutOptions (unAnnotate doc))

-- | Colored renderer (ANSI coloured)
renderErrorColor :: SaraError k -> Text
renderErrorColor e = renderStrict (layoutPretty defaultLayoutOptions (prettyError e))

-- | Render AnySaraError with colors
renderAnyErrorColor :: AnySaraError -> Text
renderAnyErrorColor (AnySaraError e) = renderErrorColor e

-- | Internal pretty printer for SaraError
prettyError :: SaraError k -> Doc AnsiStyle
prettyError e = 
  let (header, code, body, pos) = errorDetails e
      colorAnsi = errorColorAnsi e
  in vsep
    [ colorAnsi (annotate bold (pretty header <> brackets (pretty code) <> ":" <+> pretty body))
    , case pos of
        Just p -> indent 2 (colorAnsi (annotate bold "-->") <+> pretty (spFile p) <> ":" <> pretty (spLine p) <> ":" <> pretty (spColumn p))
        Nothing -> mempty
    ]

errorColorAnsi :: SaraError k -> Doc AnsiStyle -> Doc AnsiStyle
errorColorAnsi = \case
  SEOAltMissing {} -> annotate (color Yellow)
  SEOHeadingSkip {} -> annotate (color Yellow)
  SEOTitleMissing {} -> annotate (color Yellow)
  SEODescriptionMissing {} -> annotate (color Yellow)
  MigrationUnsupportedShortcode {} -> annotate (color Yellow)
  _ -> annotate (color Red)

errorDetails :: SaraError k -> (T.Text, T.Text, T.Text, Maybe SourcePos)
errorDetails = \case
  FrontmatterUnknownFormat f -> ("error", "E001", "Unknown frontmatter format in: " <> T.pack f, Nothing)
  FrontmatterParseFailure _ pos d -> ("error", "E002", d, Just pos)
  FrontmatterRemapMissing f k -> ("error", "E003", "Missing remap key '" <> k <> "' in: " <> T.pack f, Nothing)
  RouteRegexInvalid p d -> ("error", "E010", "Invalid regex pattern '" <> p <> "': " <> d, Nothing)
  RouteConflict f1 f2 out -> ("error", "E011", "Route conflict: both " <> T.pack f1 <> " and " <> T.pack f2 <> " map to " <> T.pack out, Nothing)
  MarkdownUnsupportedExtension f pos feat -> ("error", "E020", "Unsupported markdown extension '" <> feat <> "' in: " <> T.pack f, Just pos)
  TemplateNotFound t -> ("error", "E030", "Template not found: " <> T.pack t, Nothing)
  TemplateRenderFailure t d -> ("error", "E031", "Failed to render template " <> T.pack t <> ": " <> d, Nothing)
  TemplateKeyMissing t k -> ("error", "E032", "Missing key '" <> k <> "' in template: " <> T.pack t, Nothing)
  TemplateUnsafeInterpolation t ln -> ("error", "E033", "Unsafe raw interpolation detected in " <> T.pack t <> " at line " <> T.pack (show ln), Just (SourcePos t ln 0))
  SEOAltMissing _ pos src -> ("warning", "W001", "Missing alt attribute for image '" <> src <> "'", Just pos)
  SEOHeadingSkip _ pos from to -> ("warning", "W002", "Skipped heading level from " <> T.pack (show from) <> " to " <> T.pack (show to), Just pos)
  SEOTitleMissing f -> ("warning", "W003", "Missing title", Just (SourcePos f 0 0))
  SEODescriptionMissing f -> ("warning", "W004", "Missing description", Just (SourcePos f 0 0))
  ValidatorBrokenLink _ pos target -> ("error", "V001", "Broken internal link to '" <> T.pack target <> "'", Just pos)
  ValidatorMissingAsset _ pos src -> ("error", "V002", "Missing asset reference '" <> src <> "'", Just pos)
  AssetProcessingFailed f d -> ("error", "A001", "Asset processing failed for " <> T.pack f <> ": " <> d, Nothing)
  MigrationUnsupportedShortcode _ s -> ("warning", "M001", "Unsupported migration shortcode '" <> s <> "'", Nothing)
  ConfigKeyMissing k -> ("error", "C001", "Missing configuration key: " <> k, Nothing)
  SecurityPathTraversal _ att root -> ("security", "S001", "Path traversal attempt: " <> T.pack att <> " escapes " <> T.pack root, Nothing)
  SecurityGlobEscape g r -> ("security", "S002", "Invalid or escaping glob pattern '" <> g <> "': " <> r, Nothing)
  SecurityRegexReDoS p r -> ("security", "S003", "ReDoS-prone regex pattern '" <> p <> "': " <> r, Nothing)
  SecurityShellInjection p r -> ("security", "S004", "Shell injection risk in path " <> T.pack p <> ": " <> r, Nothing)
  SecurityUnsafeTemplate t ln -> ("security", "S005", "Unsafe template " <> T.pack t <> " at line " <> T.pack (show ln), Just (SourcePos t ln 0))
