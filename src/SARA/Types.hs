{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module SARA.Types
  ( ValidationState(..)
  , Item(..)
  , RouteState(..)
  , Route(..)
  , routeRegex
  , routeReplacement
  , routeLiteralPath
  , SPath
  , SafePath(..)
  , unSafePath
  , AssetFormat(..)
  , AssetKind(..)
  , ImageSpec(..)
  , ImageFormat(..)
  , formatFromText
  , GlobPattern(..)
  , SafeRegex(..)
  , SomeAssetKind(..)
  , FeedConfig(..)
  , Taxonomy(..)
  , Pager(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified BLAKE3

-- | A grouping of items by a specific metadata key (e.g., 'tags', 'categories').
data Taxonomy = Taxonomy
  { taxKey   :: !Text
  , taxMap   :: !(Map.Map Text [SPath])
  } deriving (Show, Generic)

-- | Context for a specific paginated page.
data Pager v = Pager
  { pagerItems       :: ![Item v]
  , pagerCurrent     :: !Int
  , pagerTotal       :: !Int
  , pagerPageSize    :: !Int
  , pagerHasNext     :: !Bool
  , pagerHasPrev     :: !Bool
  , pagerNextUrl     :: !(Maybe Text)
  , pagerPrevUrl     :: !(Maybe Text)
  } deriving (Show, Generic)

instance Aeson.ToJSON (Item v) => Aeson.ToJSON (Pager v) where
  toJSON p = Aeson.object
    [ "items"    Aeson..= pagerItems p
    , "current"  Aeson..= pagerCurrent p
    , "total"    Aeson..= pagerTotal p
    , "pageSize" Aeson..= pagerPageSize p
    , "hasNext"  Aeson..= pagerHasNext p
    , "hasPrev"  Aeson..= pagerHasPrev p
    , "nextUrl"  Aeson..= pagerNextUrl p
    , "prevUrl"  Aeson..= pagerPrevUrl p
    ]

-- | Industrial path type using a newtype for performance and safety.
--   A SafePath is a witness that the path has been checked against the ProjectRoot.
newtype SafePath = SafePath { unSafePath' :: FilePath }
  deriving (Show, Eq, Generic)

instance Aeson.ToJSON SafePath where
  toJSON (SafePath p) = Aeson.String (T.pack p)

type SPath = Text -- Keeping SPath for DSL backward compatibility but transitioning internals to SafePath

-- | 'v' is a phantom type: 'Planning, 'Unvalidated or 'Validated.
data ValidationState = Planning | Unvalidated | Validated

data Item (v :: ValidationState) = Item
  { itemPath     :: !SPath
  , itemRoute    :: !(Route 'Resolved)
  , itemMeta     :: !Aeson.Object
  , itemBody     :: !Text
  , itemHash     :: !(BLAKE3.Digest 256)
  } deriving (Show, Generic)

instance Aeson.ToJSON (Item v) where
  toJSON i = Aeson.object
    [ "path"  Aeson..= itemPath i
    , "route" Aeson..= itemRoute i
    , "meta"  Aeson..= itemMeta i
    , "body"  Aeson..= itemBody i
    ]

data RouteState = Abstract | Resolved

-- | Placeholder for SafeRegex until SARA.Security.RegexGuard is implemented
newtype SafeRegex = SafeRegex Text deriving (Eq, Show)

-- | A route is either a pattern (abstract) or a concrete output path (resolved).
data Route (s :: RouteState) where
  -- Abstract routes (declared in DSL)
  SlugRoute     :: Route 'Abstract
  PrettyRoute   :: Route 'Abstract
  RegexRoute    :: !SafeRegex -> !Text -> Route 'Abstract
  LiteralRoute  :: !SPath -> Route 'Abstract

  -- Resolved routes (produced by the routing engine)
  UnresolvedRoute :: Route 'Resolved
  ResolvedRoute :: !SPath -> Route 'Resolved

-- | Safe accessor for RegexRoute's SafeRegex.
routeRegex :: Route 'Abstract -> Maybe SafeRegex
routeRegex (RegexRoute r _) = Just r
routeRegex _                = Nothing

-- | Safe accessor for RegexRoute's replacement text.
routeReplacement :: Route 'Abstract -> Maybe Text
routeReplacement (RegexRoute _ t) = Just t
routeReplacement _                = Nothing

-- | Safe accessor for LiteralRoute's path.
routeLiteralPath :: Route 'Abstract -> Maybe SPath
routeLiteralPath (LiteralRoute p) = Just p
routeLiteralPath _                = Nothing

deriving instance Show (Route s)
deriving instance Eq (Route s)

instance Aeson.ToJSON (Route 'Resolved) where
  toJSON (ResolvedRoute p) = Aeson.String p
  toJSON UnresolvedRoute   = Aeson.Null

data AssetFormat
  = FormatImage
  | FormatCSS
  | FormatJS
  | FormatFont
  | FormatData
  | FormatGeneric
  deriving (Eq, Show)

data ImageFormat = WebP | AVIF | JPEG | PNG
  deriving (Eq, Show, Generic, Enum, Bounded)

-- | Helper to parse image format from text.
formatFromText :: Text -> Maybe ImageFormat
formatFromText t = case map toLower (T.unpack t) of
  "webp" -> Just WebP
  "avif" -> Just AVIF
  "jpeg" -> Just JPEG
  "jpg"  -> Just JPEG
  "png"  -> Just PNG
  _      -> Nothing

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

-- | Opaque newtype for validated glob patterns.
newtype GlobPattern = GlobPattern { unGlobPattern :: Text }
  deriving (Eq, Show)

-- | Configuration for RSS/Atom feeds.
data FeedConfig = FeedConfig
  { feedTitle       :: !Text
  , feedDescription :: !Text
  , feedAuthor      :: !Text
  , feedBaseUrl     :: !Text
  } deriving (Show, Generic)

-- | Unwrap a SafePath.
unSafePath :: SafePath -> FilePath
unSafePath = unSafePath'
