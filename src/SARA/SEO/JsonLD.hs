{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

module SARA.SEO.JsonLD
  ( JsonLDType(..)
  , generateJsonLD
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import SARA.Types (Item(..))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

data JsonLDType = WebSite | Article | BreadcrumbList
  deriving (Show, Eq, Generic)

-- | Generates a JSON-LD script tag.
generateJsonLD :: JsonLDType -> Item v -> Aeson.Value
generateJsonLD ty item = Aeson.object
  [ "@context" Aeson..= ("https://schema.org" :: Text)
  , "@type"    Aeson..= typeToText ty
  , "headline" Aeson..= (case KM.lookup (K.fromText "title") (itemMeta item) of
                            Just (Aeson.String t) -> t
                            _ -> "Untitled")
  , "author"   Aeson..= (case KM.lookup (K.fromText "author") (itemMeta item) of
                            Just (Aeson.String a) -> a
                            _ -> "Unknown")
  -- In industrial use, we would map more fields here
  ]

typeToText :: JsonLDType -> Text
typeToText = \case
  WebSite        -> "WebSite"
  Article        -> "Article"
  BreadcrumbList -> "BreadcrumbList"
