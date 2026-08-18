{-# LANGUAGE OverloadedStrings #-}

-- | Content-verifying, real-build tests for four DSL primitives that,
--   before this spec, had zero test coverage anywhere in the suite:
--   'buildSitemap', 'buildRSS', 'buildSearchIndex', and 'discover'.
--
--   This gap is exactly the kind that let 'match' sit broken
--   (always returning @[]@) for this whole codebase's history without
--   any of 150+ passing tests noticing — these four primitives are the
--   next most likely place a similar dormant bug could hide, since
--   they're the other places a real site's content flows through the
--   DSL into a generated artifact. Each test here checks real file
--   *content*, not just "a file was produced."
module SARA.ContentGenerationSpec (spec) where

import Test.Hspec
import SARA
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Exception (finally)

withTwoPostProject :: (FilePath -> IO a) -> IO a
withTwoPostProject action =
  withSystemTempDirectory "sara-content-gen" $ \tmpDir -> do
    oldCwd <- getCurrentDirectory
    (`finally` setCurrentDirectory oldCwd) $ do
      createDirectoryIfMissing True (tmpDir </> "posts")
      createDirectoryIfMissing True (tmpDir </> "templates")
      createDirectoryIfMissing True (tmpDir </> "assets")
      TIO.writeFile (tmpDir </> "templates" </> "post.html")
        "<html><head><title>{{title}}</title></head><body>{{{itemBody}}}</body></html>"
      TIO.writeFile (tmpDir </> "posts" </> "first.md")
        "---\ntitle: First Post\nauthor: Alice\n---\nThis is the first post's real body content."
      TIO.writeFile (tmpDir </> "posts" </> "second.md")
        "---\ntitle: Second Post\nauthor: Bob\n---\nThis is the second post's distinct body content."
      TIO.writeFile (tmpDir </> "assets" </> "style.css")
        "body { color: red; }"
      setCurrentDirectory tmpDir
      action tmpDir

runContentPipeline :: IO ()
runContentPipeline =
  sara $ do
    discover =<< glob "assets/*"
    postsGlob <- glob "posts/*.md"
    posts <- match postsGlob $ \file -> do
      item <- readMarkdown file
      item' <- validateSEO item
      render "templates/post.html" item'
      pure item'
    buildSitemap "sitemap.xml" posts
    buildSearchIndex "search-index.json" posts
    buildRSS "feed.xml" FeedConfig
      { feedTitle = "Test Feed"
      , feedDescription = "A test feed"
      , feedAuthor = "Tester"
      , feedBaseUrl = "http://example.test"
      } posts

spec :: Spec
spec = describe "Content generation primitives (buildSitemap/buildRSS/buildSearchIndex/discover) — real content, not just file existence" $ do

  describe "buildSitemap" $
    it "lists the real URL for every published post, not zero and not placeholders" $
      withTwoPostProject $ \tmpDir -> do
        runContentPipeline
        sitemapContent <- TIO.readFile (tmpDir </> "_site" </> "sitemap.xml")
        sitemapContent `shouldSatisfy` T.isInfixOf "first.html"
        sitemapContent `shouldSatisfy` T.isInfixOf "second.html"
        -- Exactly two <loc> entries — not zero (the match-shaped
        -- failure mode) and not duplicated.
        length (T.breakOnAll "<loc" sitemapContent) `shouldBe` 2

  describe "buildRSS" $
    it "includes both posts' real titles in the generated feed" $
      withTwoPostProject $ \tmpDir -> do
        runContentPipeline
        feedContent <- TIO.readFile (tmpDir </> "_site" </> "feed.xml")
        feedContent `shouldSatisfy` T.isInfixOf "First Post"
        feedContent `shouldSatisfy` T.isInfixOf "Second Post"

  describe "buildSearchIndex" $
    it "produces a non-empty index containing both posts' real titles and root-relative URLs" $
      withTwoPostProject $ \tmpDir -> do
        runContentPipeline
        raw <- TIO.readFile (tmpDir </> "_site" </> "search-index.json")
        raw `shouldSatisfy` T.isInfixOf "First Post"
        raw `shouldSatisfy` T.isInfixOf "Second Post"
        -- The specific, real bug this test was written looking for:
        -- every mainstream client-side search result link (see
        -- Scaffold.hs's generated search.js, which does
        -- 'location.href = res.seUrl') needs a root-relative URL to
        -- work correctly from a nested page (e.g. a tag page at
        -- /tags/x/). A URL with no leading slash is exactly the kind
        -- of thing that "looks fine" testing only from the site root
        -- and silently breaks everywhere else.
        raw `shouldSatisfy` T.isInfixOf "\"seUrl\":\"/"

  describe "discover" $
    it "copies a discovered asset into the output directory with its real content intact" $
      withTwoPostProject $ \tmpDir -> do
        runContentPipeline
        exists <- doesFileExist (tmpDir </> "_site" </> "assets" </> "style.css")
        exists `shouldBe` True
        content <- TIO.readFile (tmpDir </> "_site" </> "assets" </> "style.css")
        content `shouldSatisfy` T.isInfixOf "color: red"
