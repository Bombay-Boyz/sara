{-# LANGUAGE OverloadedStrings #-}
import SARA

main :: IO ()
main = sara $ do
  _ <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    item' <- validateSEO item
    render "templates/post.html" item'
    pure item'
  pure ()
