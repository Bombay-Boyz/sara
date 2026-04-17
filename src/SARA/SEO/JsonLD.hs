{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module SARA.SEO.JsonLD
  ( SchemaType(..)
  , generateJsonLD
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import SARA.Config (SaraConfig(..))
import GHC.Generics (Generic)

import Data.Function ((&))

data SchemaType = SchemaArticle | SchemaWebSite | SchemaWebPage
  deriving (Eq, Show)

-- | Generate JSON-LD script block.
generateJsonLD
  :: SchemaType
  -> Aeson.Object    -- ^ Item metadata (pre-escaped)
  -> SaraConfig
  -> Aeson.Value
generateJsonLD stype meta config =
  let base = KM.fromList
        [ ("@context", "https://schema.org")
        , ("@type", typeToText stype)
        ]
      specific = case stype of
        SchemaArticle -> KM.fromList
          [ ("headline", maybe (Aeson.String "Untitled") id (KM.lookup (K.fromText "title") meta))
          , ("author", KM.lookup (K.fromText "author") meta 
                       & maybe (Aeson.String (cfgSiteAuthor config)) id)
          , ("datePublished", maybe Aeson.Null id (KM.lookup (K.fromText "date") meta))
          ]
        _ -> KM.empty
  in Aeson.Object (KM.union specific base)

typeToText :: SchemaType -> Aeson.Value
typeToText = \case
  SchemaArticle -> "Article"
  SchemaWebSite -> "WebSite"
  SchemaWebPage -> "WebPage"

