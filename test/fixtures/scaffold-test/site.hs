{-# LANGUAGE OverloadedStrings #-}
import SARA

main :: IO ()
main = sara $ do
  -- Discover all static assets
  discover (glob "assets/*")

  -- Process all Markdown posts
  match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    item' <- validateSEO item
    render "templates/default.html" item'
    pure item'
