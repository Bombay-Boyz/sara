{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SARA.Types
  ( ValidationState(..)
  , ItemP(..)
  , Item
  , RouteState(..)
  , Route(..)
  , AssetFormat(..)
  , AssetKind(..)
  , ImageSpec(..)
  , ImageFormat(..)
  , GlobPattern       -- re-exported opaque; constructor lives in SARA.Security.GlobGuard (issue #2)
  , SafeRegex         -- re-exported opaque; constructor lives in SARA.Security.RegexGuard (issue #2)
  , SomeAssetKind(..)
  , FeedConfig(..)
  ) where

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import SARA.Security.GlobGuard (GlobPattern)
import SARA.Security.RegexGuard (SafeRegex)

-- | 'v' is a phantom type: 'Unvalidated or 'Validated.
data ValidationState = Unvalidated | Validated

-- | A content item, generalised over its metadata type @meta@.
--
--   Most of this codebase, and most SARA sites, use the default
--   'Item' alias below, where @meta@ is a raw 'Aeson.Object' — the
--   metadata bag frontmatter naturally parses into, with no schema
--   assumed. That default is exactly as flexible as before this type
--   was parameterised: nothing about existing code changes, because
--   'Item' still means what it always meant.
--
--   The parameter exists for callers who want more: given a metadata
--   record with a 'Aeson.FromJSON' instance, 'SARA.DSL.readMarkdownAs'
--   decodes frontmatter directly into it, so
--
--   > data BlogPost = BlogPost { bpTitle :: Text, bpTags :: [Text] }
--   >   deriving (Generic, FromJSON)
--   > item <- readMarkdownAs @BlogPost "posts/hello.md"
--   > bpTitle (itemMeta item)   -- a real field, checked at compile time
--
--   replaces the untyped @KM.lookup "title" (itemMeta item)@ every
--   other consumer of frontmatter still has to write and can get
--   wrong (missing key, wrong JSON shape) without the compiler's help.
--   This is the same shape Hakyll's own @Item a@ uses for a page's
--   body; here it's applied to metadata, which is the part of a
--   content item that most wants a schema and, in the untyped-JSON
--   design this replaces, had none at all.
data ItemP (v :: ValidationState) meta = Item
  { itemPath     :: !FilePath
  , itemRoute    :: !(Route 'Resolved)
  , itemMeta     :: !meta
  , itemBody     :: !Text
  , itemHash     :: !Text
  }

-- | The default, schema-less item: metadata as a raw JSON object,
--   exactly as every 'Item' in this codebase behaved before 'ItemP'
--   existed. Every existing use of @Item 'Validated@ /
--   @Item 'Unvalidated@ continues to typecheck unchanged.
type Item v = ItemP v Aeson.Object

data RouteState = Abstract | Resolved


-- | A route is either a pattern (abstract) or a concrete output path (resolved).
data Route (s :: RouteState) where
  -- Abstract routes (declared in DSL)
  SlugRoute   :: Route 'Abstract
  PrettyRoute :: Route 'Abstract
  RegexRoute
    :: { rrSafeRegex     :: !SafeRegex
       , rrReplacement   :: !Text
       }
    -> Route 'Abstract
  LiteralRoute
    :: { lrPath :: !FilePath }
    -> Route 'Abstract

  -- Resolved routes (produced by the routing engine)
  ResolvedRoute
    :: { resolvedPath :: !FilePath
       }
    -> Route 'Resolved

deriving instance Show (Route s)
deriving instance Eq (Route s)

data AssetFormat
  = FormatImage
  | FormatCSS
  | FormatJS
  | FormatFont
  | FormatData
  | FormatGeneric
  deriving (Eq, Show)

data ImageFormat = WebP | AVIF | JPEG | PNG
  deriving (Eq, Show, Generic)

data ImageSpec = ImageSpec
  { imgWidths  :: ![Int]
  , imgFormats :: ![ImageFormat]
  , imgQuality :: !Int
  } deriving (Eq, Show, Generic)

data AssetKind (a :: AssetFormat) where
  ImageAsset   :: ImageSpec -> AssetKind 'FormatImage
  StyleAsset   ::              AssetKind 'FormatCSS
  ScriptAsset  ::              AssetKind 'FormatJS
  FontAsset    ::              AssetKind 'FormatFont
  DataAsset    ::              AssetKind 'FormatData
  GenericAsset ::              AssetKind 'FormatGeneric

deriving instance Show (AssetKind a)
deriving instance Eq (AssetKind a)

data SomeAssetKind where
  SomeAssetKind :: AssetKind a -> SomeAssetKind

deriving instance Show SomeAssetKind

-- | Configuration for RSS/Atom feeds.
data FeedConfig = FeedConfig
  { feedTitle       :: !Text
  , feedDescription :: !Text
  , feedAuthor      :: !Text
  , feedBaseUrl     :: !Text
  } deriving (Show, Generic)
