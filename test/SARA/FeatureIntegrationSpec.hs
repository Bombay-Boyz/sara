{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end proof that drafts, taxonomy pages, and pagination
--   actually work together through a real build — not just their
--   individual pure helper functions in isolation (which
--   'SARA.DraftsSpec'\/'SARA.TaxonomySpec'\/'SARA.PaginationSpec'
--   already cover). This is also, incidentally, the test that would
--   have caught the 'match' bug this session found (it always
--   returned @[]@): every pre-existing test discarded 'match's return
--   value, and this one depends on it being correct throughout.
module SARA.FeatureIntegrationSpec (spec) where

import Test.Hspec
import SARA
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Exception (finally)

withFeatureProject :: (FilePath -> IO a) -> IO a
withFeatureProject action =
  withSystemTempDirectory "sara-feature-integration" $ \tmpDir -> do
    oldCwd <- getCurrentDirectory
    (`finally` setCurrentDirectory oldCwd) $ do
      createDirectoryIfMissing True (tmpDir </> "posts")
      createDirectoryIfMissing True (tmpDir </> "templates")

      TIO.writeFile (tmpDir </> "templates" </> "post.html")
        "<html><head><title>{{title}}</title></head><body>{{{itemBody}}}</body></html>"
      TIO.writeFile (tmpDir </> "templates" </> "tags.html")
        "<html><head><title>Tag: {{term}}</title></head><body>{{#posts}}<a href=\"{{url}}\">{{title}}</a>{{/posts}}</body></html>"
      TIO.writeFile (tmpDir </> "templates" </> "index.html")
        "<html><head><title>Blog</title></head><body>page {{pageNumber}}/{{totalPages}} {{#posts}}<a href=\"{{url}}\">{{title}}</a>{{/posts}} {{#hasNext}}<a href=\"{{nextUrl}}\">next</a>{{/hasNext}} {{#hasPrev}}<a href=\"{{prevUrl}}\">prev</a>{{/hasPrev}}</body></html>"

      let mkPost name meta body =
            TIO.writeFile (tmpDir </> "posts" </> name)
              (T.unlines (["---"] ++ meta ++ ["---", body]))

      mkPost "post-1.md" ["title: Post One", "date: 2020-01-01", "tags: [haskell, tutorial]"] "Body one."
      mkPost "post-2.md" ["title: Post Two", "date: 2020-01-02", "tags: [haskell]"] "Body two."
      mkPost "post-3.md" ["title: Post Three", "date: 2020-01-03", "tags: [rust]"] "Body three."
      mkPost "draft.md"  ["title: Secret Draft", "date: 2020-01-01", "draft: true", "tags: [haskell]"] "Should never appear."
      mkPost "future.md" ["title: From The Future", "date: 2099-01-01", "tags: [haskell]"] "Should never appear either."

      setCurrentDirectory tmpDir
      action tmpDir

runFeaturePipeline :: IO ()
runFeaturePipeline =
  sara $ do
    allPosts <- match (glob "posts/*.md") $ \file -> do
      item <- readMarkdown file
      validateSEO item
    published <- filterPublished allPosts
    mapM_ (render "templates/post.html") published
    _ <- buildTaxonomyPages "tags" "templates/tags.html" "tags" published
    _ <- buildPaginatedIndex "templates/index.html" 2 "" published
    pure ()

spec :: Spec
spec = describe "Drafts + Taxonomy + Pagination, end-to-end through a real build" $ do

  it "match returns the real matched items (regression guard for the session's own match-always-[] bug)" $
    withFeatureProject $ \tmpDir -> do
      runFeaturePipeline
      -- If match still returned [], none of the assertions below
      -- would have anything to find — this first check makes that
      -- failure mode explicit rather than implicit in the others.
      postOneExists <- doesFileExist (tmpDir </> "_site" </> "posts" </> "post-1.html")
      postOneExists `shouldBe` True

  it "excludes drafts and future-dated posts from rendered output" $
    withFeatureProject $ \tmpDir -> do
      runFeaturePipeline
      draftExists  <- doesFileExist (tmpDir </> "_site" </> "posts" </> "draft.html")
      futureExists <- doesFileExist (tmpDir </> "_site" </> "posts" </> "future.html")
      draftExists  `shouldBe` False
      futureExists `shouldBe` False

  it "renders all three published posts" $
    withFeatureProject $ \tmpDir -> do
      runFeaturePipeline
      mapM_ (\n -> doesFileExist (tmpDir </> "_site" </> "posts" </> n) >>= (`shouldBe` True))
        ["post-1.html", "post-2.html", "post-3.html"]

  it "renders one taxonomy page per tag, listing only published posts that carry that tag" $
    withFeatureProject $ \tmpDir -> do
      runFeaturePipeline
      haskellTagContent <- TIO.readFile (tmpDir </> "_site" </> "tags" </> "haskell" </> "index.html")
      -- post-1 and post-2 carry "haskell" and are published; the draft
      -- and future post also carry "haskell" but must not appear.
      haskellTagContent `shouldSatisfy` T.isInfixOf "Post One"
      haskellTagContent `shouldSatisfy` T.isInfixOf "Post Two"
      haskellTagContent `shouldNotSatisfy` T.isInfixOf "Post Three"
      haskellTagContent `shouldNotSatisfy` T.isInfixOf "Secret Draft"
      haskellTagContent `shouldNotSatisfy` T.isInfixOf "From The Future"

      rustTagContent <- TIO.readFile (tmpDir </> "_site" </> "tags" </> "rust" </> "index.html")
      rustTagContent `shouldSatisfy` T.isInfixOf "Post Three"
      rustTagContent `shouldNotSatisfy` T.isInfixOf "Post One"

  it "paginates the published posts (3 posts, page size 2 -> 2 pages) with correct prev/next links" $
    withFeatureProject $ \tmpDir -> do
      runFeaturePipeline
      page1 <- TIO.readFile (tmpDir </> "_site" </> "index.html")
      page2 <- TIO.readFile (tmpDir </> "_site" </> "page" </> "2" </> "index.html")

      page1 `shouldSatisfy` T.isInfixOf "page 1/2"
      page1 `shouldSatisfy` T.isInfixOf "next"
      page1 `shouldNotSatisfy` T.isInfixOf "prev"

      page2 `shouldSatisfy` T.isInfixOf "page 2/2"
      page2 `shouldSatisfy` T.isInfixOf "prev"
      page2 `shouldNotSatisfy` T.isInfixOf "next"

      thirdPageExists <- doesFileExist (tmpDir </> "_site" </> "page" </> "3" </> "index.html")
      thirdPageExists `shouldBe` False
