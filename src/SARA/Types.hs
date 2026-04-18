{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric  #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module SARA.Types
  ( ValidationState(..)
  , Item(..)
  , RouteState(..)
  , Route(..)
  , SPath
  , AssetFormat(..)
  , AssetKind(..)
  , ImageSpec(..)
  , ImageFormat(..)
  , GlobPattern(..)
  , SafeRegex(..)
  , SomeAssetKind(..)
  , FeedConfig(..)
  ) where

import Data.Text (Text)
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified BLAKE3

-- | Industrial path type using Text for performance and safety.
type SPath = Text

-- | 'v' is a phantom type: 'Planning, 'Unvalidated or 'Validated.
data ValidationState = Planning | Unvalidated | Validated

data Item (v :: ValidationState) = Item
  { itemPath     :: !SPath
  , itemRoute    :: !(Route 'Resolved)
  , itemMeta     :: !Aeson.Object
  , itemBody     :: !Text
  , itemHash     :: !(BLAKE3.Digest 256)
  }

data RouteState = Abstract | Resolved

-- | Placeholder for SafeRegex until SARA.Security.RegexGuard is implemented
newtype SafeRegex = SafeRegex Text deriving (Eq, Show)

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
    :: { lrPath :: !SPath }
    -> Route 'Abstract

  -- Resolved routes (produced by the routing engine)
  ResolvedRoute
    :: { resolvedPath :: !SPath
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
