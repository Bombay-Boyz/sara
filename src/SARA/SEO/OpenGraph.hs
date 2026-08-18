{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.OpenGraph
  ( generateOGTags
  , generateTwitterTags
  ) where

import qualified Data.Aeson as Aeson
import Data.Text (Text)
import SARA.Internal.Aeson (lookupText)

-- | Generates OpenGraph meta tags from item metadata.
generateOGTags
  :: Aeson.Object
  -> [(Text, Text)]
generateOGTags meta = 
  let title = lookupText "title" meta
      desc  = lookupText "description" meta
      image = lookupText "image" meta
  in concat
     [ maybe [] (\t -> [("og:title", t)]) title
     , maybe [] (\d -> [("og:description", d)]) desc
     , maybe [] (\i -> [("og:image", i)]) image
     , [("og:type", "website")]
     ]

-- | Generates Twitter Card meta tags from item metadata.
generateTwitterTags
  :: Aeson.Object
  -> [(Text, Text)]
generateTwitterTags meta =
  let title = lookupText "title" meta
      desc  = lookupText "description" meta
  in concat
     [ maybe [] (\t -> [("twitter:title", t)]) title
     , maybe [] (\d -> [("twitter:description", d)]) desc
     , [("twitter:card", "summary_large_image")]
     ]
