{-# LANGUAGE OverloadedStrings #-}

module SARA.SEO.Feed
  ( generateRSS
  ) where

import SARA.Types (Item(..), Route(..), FeedConfig(..))
import qualified Data.Text as T
import qualified Text.XML as XML
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Map as Map
import Development.Shake (Action, liftIO)

-- | Generates an RSS 2.0 feed.
generateRSS
  :: FeedConfig
  -> [Item v]
  -> FilePath
  -> Action ()
generateRSS cfg items outPath = do
  let channelNodes = 
        [ XML.NodeElement $ XML.Element "title" Map.empty [XML.NodeContent (feedTitle cfg)]
        , XML.NodeElement $ XML.Element "link" Map.empty [XML.NodeContent (feedBaseUrl cfg)]
        , XML.NodeElement $ XML.Element "description" Map.empty [XML.NodeContent (feedDescription cfg)]
        ] ++ map (XML.NodeElement . mkItem cfg) items
      root = XML.Element "rss" (Map.fromList [("version", "2.0")]) [XML.NodeElement $ XML.Element "channel" Map.empty channelNodes]
      doc = XML.Document (XML.Prologue [] Nothing []) root []
  liftIO $ XML.writeFile XML.def outPath doc

mkItem :: FeedConfig -> Item v -> XML.Element
mkItem cfg item =
  let path = case itemRoute item of ResolvedRoute p -> T.pack p
      loc = feedBaseUrl cfg <> if "/" `T.isPrefixOf` path then path else "/" <> path
      title = case KM.lookup (K.fromText "title") (itemMeta item) of
                Just (Aeson.String t) -> t
                _ -> "Untitled"
  in XML.Element "item" Map.empty
       [ XML.NodeElement $ XML.Element "title" Map.empty [XML.NodeContent title]
       , XML.NodeElement $ XML.Element "link" Map.empty [XML.NodeContent loc]
       , XML.NodeElement $ XML.Element "guid" Map.empty [XML.NodeContent loc]
       , XML.NodeElement $ XML.Element "description" Map.empty [XML.NodeContent (itemBody item)] -- Should ideally be an excerpt
       ]
