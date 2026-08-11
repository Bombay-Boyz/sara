{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.JsonLD
  ( SchemaType(..)
  , generateJsonLD
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import SARA.Config (SaraConfig(..))

data SchemaType = SchemaArticle | SchemaWebSite | SchemaWebPage
  deriving (Eq, Show)

-- | Generate JSON-LD script block. 'meta' is taken as-is: JSON-LD is
--   embedded in a @<script type="application/ld+json">@ block, which
--   is JSON syntax, not HTML content — HTML-escaping a value here
--   would corrupt it (e.g. a headline containing @&@ would wrongly
--   become @&amp;@ inside the JSON string). Aeson's own encoder already
--   handles JSON-string escaping, which is the only escaping this
--   output format needs.
generateJsonLD
  :: SchemaType
  -> Aeson.Object    -- ^ Item metadata, as authored
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
                       |> maybe (Aeson.String (cfgSiteAuthor config)) id)
          , ("datePublished", maybe Aeson.Null id (KM.lookup (K.fromText "date") meta))
          ]
        _ -> KM.empty
  in Aeson.Object (KM.union specific base)

typeToText :: SchemaType -> Aeson.Value
typeToText = \case
  SchemaArticle -> "Article"
  SchemaWebSite -> "WebSite"
  SchemaWebPage -> "WebPage"

(|>) :: a -> (a -> b) -> b
x |> f = f x
infixl 0 |>
