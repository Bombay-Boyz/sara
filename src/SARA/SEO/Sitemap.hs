{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.Sitemap
  ( generateSitemap
  ) where

import SARA.Types (Item(..), Route(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Data.Map as Map
import Development.Shake (Action, liftIO)

-- | Generates a sitemap.xml from validated items.
generateSitemap
  :: Text -- ^ Site Base URL
  -> [Item v]
  -> FilePath -- ^ Output path
  -> Action ()
generateSitemap baseUrl items outPath = do
  let urls = map (mkUrl baseUrl) items
  let sitemap = XML.Document (XML.Prologue [] Nothing []) root []
      root = XML.Element "{http://www.sitemaps.org/schemas/sitemap/0.9}urlset" Map.empty (map XML.NodeElement urls)
  liftIO $ XML.writeFile XML.def outPath sitemap

mkUrl :: Text -> Item v -> XML.Element
mkUrl baseUrl item =
  let path = case itemRoute item of ResolvedRoute p -> T.pack p
      loc = baseUrl <> if "/" `T.isPrefixOf` path then path else "/" <> path
  in XML.Element "{http://www.sitemaps.org/schemas/sitemap/0.9}url" Map.empty
       [ XML.NodeElement $ XML.Element "{http://www.sitemaps.org/schemas/sitemap/0.9}loc" Map.empty [XML.NodeContent loc] ]
