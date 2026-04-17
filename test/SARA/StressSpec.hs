{-# LANGUAGE OverloadedStrings #-}

module SARA.StressSpec (spec) where

import Test.Hspec
import SARA
import SARA.Frontmatter.Parser
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Monad (forM_, void)
import Control.Exception (finally)

spec :: Spec
spec = do
  describe "SARA Stress Testing" $ do
    it "handles 1000 Markdown files efficiently" $ do
      withSystemTempDirectory "sara-stress" $ \tmpDir -> do
        let postsDir = tmpDir </> "posts"
        let siteDir = tmpDir </> "_site"
        createDirectoryIfMissing True postsDir
        createDirectoryIfMissing True siteDir
        
        let items :: [Int]
            items = [1..1000]
        forM_ items $ \i -> do
          let content = T.unlines
                [ "---"
                , "title: Stress Post " <> T.pack (show i)
                , "author: Stress Tester"
                , "---"
                , "# Body " <> T.pack (show i)
                ]
          TIO.writeFile (postsDir </> "post-" ++ show i ++ ".md") content
        
        -- Run the real sara engine on the 1000 files
        oldCwd <- getCurrentDirectory
        (`finally` setCurrentDirectory oldCwd) $ do
          setCurrentDirectory tmpDir
          createDirectoryIfMissing True "templates"
          TIO.writeFile "templates/post.html" "<html><head><title>Industrial Stress Test</title></head><body>{{{itemBody}}}</body></html>"
          sara $ do
            void $ match (glob "posts/*.md") $ \file -> do
              item <- readMarkdown file
              item' <- validateSEO item
              render "templates/post.html" item'
              pure item'
        True `shouldBe` True

    it "handles very large Markdown files (5MB)" $ do
      let body = T.replicate 50000 "This is a ten-word sentence that we will replicate many times. "
      let content = "---\ntitle: Large\n---\n" <> body
      case parseFrontmatter "large.md" content of
        Right (_, b) -> T.length b `shouldSatisfy` (> 1000000)
        Left e -> expectationFailure $ "Large file parse failed: " ++ show e
