{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Integration.IntegrationSpec (spec) where

import Test.Hspec
import SARA
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, getCurrentDirectory, setCurrentDirectory)
import Control.Exception (bracket_)

spec :: Spec
spec = do
  describe "Build Integration" $ do
    it "builds a minimal site correctly" $ do
      withTestSite $ do
        sara minimalSite
        -- Verify output
        content <- readFile "_site/posts/hello.html"
        content `shouldBe` "<html><head><title>Hello</title><meta name=\"description\" content=\"A very detailed description of this wonderful site that meets length requirements.\"><meta property=\"og:title\" content=\"Hello\"><meta property=\"og:image\" content=\"img.png\"></head><body><h1>Hello</h1>\n</body></html>"

withTestSite :: IO a -> IO a
withTestSite action = do
  curr <- getCurrentDirectory
  let tmpDir = "_test_tmp"
  bracket_ 
    (do
      createDirectoryIfMissing True tmpDir
      setCurrentDirectory tmpDir
      createDirectoryIfMissing True "posts"
      createDirectoryIfMissing True "templates"
      writeFile "posts/hello.md" "---\ntitle: Hello\n---\n# Hello"
      writeFile "templates/post.html" "<html><head><title>{{ title }}</title><meta name=\"description\" content=\"A very detailed description of this wonderful site that meets length requirements.\"><meta property=\"og:title\" content=\"{{ title }}\"><meta property=\"og:image\" content=\"img.png\"></head><body>{{{ itemBody }}}</body></html>"
    )
    (do
      setCurrentDirectory curr
      removeDirectoryRecursive tmpDir
    )
    action

minimalSite :: SaraM ()
minimalSite = do
  _ <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    validated <- validateSEO item
    render "templates/post.html" validated
    pure validated
  return ()
