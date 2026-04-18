{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric  #-}
module SARA.Types
  ( ValidationState(..)
  , Item(..)
  , RouteState(..)
  , Route(..)
  , routeRegex
  , routeReplacement
  , routeLiteralPath
  , SPath
  , AssetFormat(..)
  , AssetKind(..)
  , ImageSpec(..)
  , ImageFormat(..)
  , GlobPattern(..)
  , SafeRegex(..)
  , SomeAssetKind(..)
  , FeedConfig(..)
  , Taxonomy(..)
  ) where

import Data.Text (Text)
import qualified Data.Map.Strict as Map
import qualified Data.Aeson as Aeson
import GHC.Generics (Generic)
import qualified BLAKE3

-- | A grouping of items by a specific metadata key (e.g., 'tags', 'categories').
data Taxonomy = Taxonomy
  { taxKey   :: !Text
  , taxMap   :: !(Map.Map Text [SPath])
  } deriving (Show, Generic)

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
