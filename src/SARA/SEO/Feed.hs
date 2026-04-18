{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.SEO.Feed
  ( generateRSS
  ) where

import SARA.Types (Item(..), FeedConfig(..), Route(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

-- | Generates an RSS 2.0 feed.
generateRSS :: FeedConfig -> [Item v] -> FilePath -> IO ()
generateRSS cfg items outPath = do
  let content = T.unlines
        [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        , "<rss version=\"2.0\">"
        , "  <channel>"
        , "    <title>" <> feedTitle cfg <> "</title>"
        , "    <description>" <> feedDescription cfg <> "</description>"
        , "    <link>" <> feedBaseUrl cfg <> "</link>"
        , T.unlines (map (renderItem cfg) items)
        , "  </channel>"
        , "</rss>"
        ]
  TIO.writeFile outPath content

renderItem :: FeedConfig -> Item v -> Text
renderItem cfg item =
  let path = case itemRoute item of
               ResolvedRoute p -> p
               UnresolvedRoute -> ""
      fullUrl = if "/" `T.isSuffixOf` feedBaseUrl cfg
                then feedBaseUrl cfg <> path
                else feedBaseUrl cfg <> "/" <> path
      title = case KM.lookup (K.fromText "title") (itemMeta item) of
                Just (Aeson.String t) -> t
                _ -> "Untitled"
  in T.unlines
    [ "    <item>"
    , "      <title>" <> title <> "</title>"
    , "      <link>" <> fullUrl <> "</link>"
    , "      <guid>" <> fullUrl <> "</guid>"
    , "    </item>"
    ]
