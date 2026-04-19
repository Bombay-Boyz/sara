{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Control.Exception (Exception)
import SARA.Types (SPath)

-- | Source location carried by every error that originates in user content.
data SourcePos = SourcePos
  { spFile   :: !SPath
  , spLine   :: !Int
  , spColumn :: !Int
  } deriving (Eq, Show, Generic)

instance Aeson.ToJSON SourcePos
instance Aeson.FromJSON SourcePos

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
    :: !SPath
    -> SaraError 'EKFrontmatter
  FrontmatterParseFailure
    :: !SPath -> !SourcePos -> !Text
    -> SaraError 'EKFrontmatter
  FrontmatterRemapMissing
    :: !SPath -> !Text
    -> SaraError 'EKFrontmatter

  -- Routing errors
  RouteRegexInvalid
    :: !Text -> !Text
    -> SaraError 'EKRouting
  RouteConflict
    :: !SPath -> !SPath -> !FilePath
    -> SaraError 'EKRouting

  -- Markdown errors
  MarkdownUnsupportedExtension
    :: !SPath -> !SourcePos -> !Text
    -> SaraError 'EKMarkdown

  -- Template errors
  TemplateNotFound
    :: !SPath
    -> SaraError 'EKTemplate
  TemplateCompileError
    :: !SPath -> !(Maybe Int) -> !(Maybe Int) -> !Text
    -> SaraError 'EKTemplate
  TemplateRenderFailure
    :: !SPath -> !(Maybe Int) -> !(Maybe Int) -> !Text
    -> SaraError 'EKTemplate
  TemplateKeyMissing
    :: !SPath -> !Text
    -> SaraError 'EKTemplate
  TemplateUnsafeInterpolation
    :: !SPath -> !Int
    -> SaraError 'EKTemplate

  -- SEO errors
  SEOAltMissing
    :: !SPath -> !SourcePos -> !Text
    -> SaraError 'EKSEO
  SEOHeadingSkip
    :: !SPath -> !SourcePos -> !Int -> !Int
    -> SaraError 'EKSEO
  SEOTitleMissing
    :: !SPath
    -> SaraError 'EKSEO
  SEODescriptionMissing
    :: !SPath
    -> SaraError 'EKSEO
  SEOGenericWarning
    :: !SPath -> !SourcePos -> !Text
    -> SaraError 'EKSEO

  -- Validator errors
  ValidatorBrokenLink
    :: !SPath -> !SourcePos -> !FilePath
    -> SaraError 'EKValidator
  ValidatorMissingAsset
    :: !SPath -> !SourcePos -> !Text
    -> SaraError 'EKValidator

  -- Asset errors
  AssetProcessingFailed
    :: !FilePath -> !Text
    -> SaraError 'EKAsset

  -- Migration errors
  MigrationUnsupportedShortcode
    :: !SPath -> !Text
    -> SaraError 'EKMigration

  -- Config errors
  ConfigKeyMissing
    :: !Text
    -> SaraError 'EKConfig

  -- Security errors
  SecurityPathTraversal
    :: !SPath -> !FilePath -> !FilePath
    -> SaraError 'EKSecurity
  SecurityGlobEscape
    :: !Text -> !Text
    -> SaraError 'EKSecurity
  SecurityRegexReDoS
    :: !Text -> !Text
    -> SaraError 'EKSecurity
  SecurityShellInjection
    :: !FilePath -> !Text
    -> SaraError 'EKSecurity
  SecurityUnsafeTemplate
    :: !SPath -> !Int
    -> SaraError 'EKSecurity

-- | Existential wrapper so errors from all subsystems can be collected.
data AnySaraError where
  AnySaraError :: SaraError k -> AnySaraError

instance Aeson.ToJSON AnySaraError where
  toJSON (AnySaraError e) =
    let (header, code, body, pos) = errorDetails e
    in Aeson.object
      [ "level"   Aeson..= header
      , "code"    Aeson..= code
      , "message" Aeson..= body
      , "pos"     Aeson..= pos
      ]

instance Exception AnySaraError

deriving instance Show AnySaraError

-- Manual Eq instance for existential wrapper
instance Eq AnySaraError where
  (AnySaraError e1) == (AnySaraError e2) = 
    case (e1, e2) of
      (FrontmatterUnknownFormat f1, FrontmatterUnknownFormat f2) -> f1 == f2
      (FrontmatterParseFailure f1 p1 d1, FrontmatterParseFailure f2 p2 d2) -> f1 == f2 && p1 == p2 && d1 == d2
      (FrontmatterRemapMissing f1 k1, FrontmatterRemapMissing f2 k2) -> f1 == f2 && k1 == k2
      (RouteRegexInvalid p1 d1, RouteRegexInvalid p2 d2) -> p1 == p2 && d1 == d2
      (RouteConflict f1a f1b o1, RouteConflict f2a f2b o2) -> f1a == f2a && f1b == f2b && o1 == o2
      (MarkdownUnsupportedExtension f1 p1 fe1, MarkdownUnsupportedExtension f2 p2 fe2) -> f1 == f2 && p1 == p2 && fe1 == fe2
      (TemplateNotFound t1, TemplateNotFound t2) -> t1 == t2
      (TemplateCompileError t1 l1 c1 d1, TemplateCompileError t2 l2 c2 d2) -> t1 == t2 && l1 == l2 && c1 == c2 && d1 == d2
      (TemplateRenderFailure t1 l1 c1 d1, TemplateRenderFailure t2 l2 c2 d2) -> t1 == t2 && l1 == l2 && c1 == c2 && d1 == d2
      (TemplateKeyMissing t1 k1, TemplateKeyMissing t2 k2) -> t1 == t2 && k1 == k2
      (TemplateUnsafeInterpolation t1 l1, TemplateUnsafeInterpolation t2 l2) -> t1 == t2 && l1 == l2
      (SEOAltMissing f1 p1 s1, SEOAltMissing f2 p2 s2) -> f1 == f2 && p1 == p2 && s1 == s2
      (SEOHeadingSkip f1 p1 fr1 t1, SEOHeadingSkip f2 p2 fr2 t2) -> f1 == f2 && p1 == p2 && fr1 == fr2 && t1 == t2
      (SEOTitleMissing f1, SEOTitleMissing f2) -> f1 == f2
      (SEODescriptionMissing f1, SEODescriptionMissing f2) -> f1 == f2
      (ValidatorBrokenLink f1 p1 t1, ValidatorBrokenLink f2 p2 t2) -> f1 == f2 && p1 == p2 && t1 == t2
      (ValidatorMissingAsset f1 p1 s1, ValidatorMissingAsset f2 p2 s2) -> f1 == f2 && p1 == p2 && s1 == s2
      (AssetProcessingFailed f1 d1, AssetProcessingFailed f2 d2) -> f1 == f2 && d1 == d2
      (MigrationUnsupportedShortcode f1 s1, MigrationUnsupportedShortcode f2 s2) -> f1 == f2 && s1 == s2
      (ConfigKeyMissing k1, ConfigKeyMissing k2) -> k1 == k2
      (SecurityPathTraversal f1 a1 r1, SecurityPathTraversal f2 a2 r2) -> f1 == f2 && a1 == a2 && r1 == r2
      (SecurityGlobEscape g1 r1, SecurityGlobEscape g2 r2) -> g1 == g2 && r1 == r2
      (SecurityRegexReDoS p1 r1, SecurityRegexReDoS p2 r2) -> p1 == p2 && r1 == r2
      (SecurityShellInjection p1 r1, SecurityShellInjection p2 r2) -> p1 == p2 && r1 == r2
      (SecurityUnsafeTemplate t1 l1, SecurityUnsafeTemplate t2 l2) -> t1 == t2 && l1 == l2
      _ -> False

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
  SEOGenericWarning {} -> annotate (color Yellow)
  MigrationUnsupportedShortcode {} -> annotate (color Yellow)
  _ -> annotate (color Red)

errorDetails :: SaraError k -> (Text, Text, Text, Maybe SourcePos)
errorDetails = \case
  FrontmatterUnknownFormat f -> ("error", "E001", "Unknown frontmatter format in: " <> f, Nothing)
  FrontmatterParseFailure _ pos d -> ("error", "E002", d, Just pos)
  FrontmatterRemapMissing f k -> ("error", "E003", "Missing remap key '" <> k <> "' in: " <> f, Nothing)
  RouteRegexInvalid p d -> ("error", "E010", "Invalid regex pattern '" <> p <> "': " <> d, Nothing)
  RouteConflict f1 f2 out -> ("error", "E011", "Route conflict: both " <> f1 <> " and " <> f2 <> " map to " <> T.pack out, Nothing)
  MarkdownUnsupportedExtension f pos feat -> ("error", "E020", "Unsupported markdown extension '" <> feat <> "' in: " <> f, Just pos)
  TemplateNotFound t -> ("error", "E030", "Template not found: " <> t, Nothing)
  TemplateCompileError t ln col d -> 
    let pos = SourcePos t <$> ln <*> col
    in ("error", "E034", "Template compilation failed: " <> d, pos)
  TemplateRenderFailure t ln col d -> 
    let pos = SourcePos t <$> ln <*> col
    in ("error", "E031", "Template rendering failed: " <> d, pos)
  TemplateKeyMissing t k -> ("error", "E032", "Missing key '" <> k <> "' in template: " <> t, Nothing)
  TemplateUnsafeInterpolation t ln -> ("error", "E033", "Unsafe raw interpolation detected in " <> t <> " at line " <> T.pack (show ln), Just (SourcePos t ln 0))
  SEOAltMissing _ pos src -> ("warning", "W001", "Missing alt attribute for image '" <> src <> "'", Just pos)
  SEOHeadingSkip _ pos from to -> ("warning", "W002", "Skipped heading level from " <> T.pack (show from) <> " to " <> T.pack (show to), Just pos)
  SEOTitleMissing f -> ("warning", "W003", "Missing title in: " <> f, Nothing)
  SEODescriptionMissing f -> ("warning", "W004", "Missing description in: " <> f, Nothing)
  SEOGenericWarning _ pos msg -> ("warning", "W005", msg, Just pos)
  ValidatorBrokenLink _ pos target -> ("error", "V001", "Broken internal link to '" <> T.pack target <> "'", Just pos)
  ValidatorMissingAsset _ pos src -> ("error", "V002", "Missing asset reference '" <> src <> "'", Just pos)
  AssetProcessingFailed f d -> ("error", "A001", "Asset processing failed for " <> T.pack f <> ": " <> d, Nothing)
  MigrationUnsupportedShortcode _ s -> ("warning", "M001", "Unsupported migration shortcode '" <> s <> "'", Nothing)
  ConfigKeyMissing k -> ("error", "C001", "Missing configuration key: " <> k, Nothing)
  SecurityPathTraversal _ att root -> ("security", "S001", "Path traversal attempt: " <> T.pack att <> " escapes " <> T.pack root, Nothing)
  SecurityGlobEscape g r -> ("security", "S002", "Invalid or escaping glob pattern '" <> g <> "': " <> r, Nothing)
  SecurityRegexReDoS p r -> ("security", "S003", "ReDoS-prone regex pattern '" <> p <> "': " <> r, Nothing)
  SecurityShellInjection p r -> ("security", "S004", "Shell injection risk in path " <> T.pack p <> ": " <> r, Nothing)
  SecurityUnsafeTemplate t ln -> ("security", "S005", "Unsafe template " <> t <> " at line " <> T.pack (show ln), Just (SourcePos t ln 0))
