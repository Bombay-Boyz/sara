{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.Sitemap
  ( generateSitemap
  ) where

import SARA.Types (Item(..), SPath)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import SARA.Types (Route(..))

-- | Generates a sitemap.xml file.
generateSitemap :: Text -> [Item v] -> FilePath -> IO ()
generateSitemap baseUrl items outPath = do
  let content = T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<urlset xmlns=\"http://www.sitemaps.org/schemas/sitemap/0.9\">"
        , T.unlines (map (renderUrl baseUrl) items)
        , "</urlset>"
        ]
  TIO.writeFile outPath content

renderUrl :: Text -> Item v -> Text
renderUrl baseUrl item =
  let path = case itemRoute item of
               ResolvedRoute p -> p
      fullUrl = if "/" `T.isSuffixOf` baseUrl
                then baseUrl <> path
                else baseUrl <> "/" <> path
  in T.unlines
    [ "  <url>"
    , "    <loc>" <> fullUrl <> "</loc>"
    , "  </url>"
    ]
