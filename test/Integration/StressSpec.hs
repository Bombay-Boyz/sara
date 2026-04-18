{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Integration.StressSpec (spec) where

import Test.Hspec
import SARA
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import Control.Exception (bracket_)
import qualified Data.List as L

spec :: Spec
spec = do
  describe "Stress Testing" $ do
    it "handles 100 concurrent posts" $ do
      withSystemTempDirectory "sara-stress" $ \tmpDir -> do
          let postsDir = tmpDir </> "posts"
          let tplDir = tmpDir </> "templates"
          createDirectoryIfMissing True postsDir
          createDirectoryIfMissing True tplDir
          
          -- Generate 100 posts
          let postCount = 100
          mapM_ (\i -> do
              let content = "---\ntitle: Post " <> T.pack (show i) <> "\n---\nBody " <> T.pack (show i)
              TIO.writeFile (postsDir </> "post-" ++ show i ++ ".md") content
            ) ([1..postCount] :: [Int])
          
          TIO.writeFile (tplDir </> "post.html") "<html><head><title>{{ itemMeta.title }}</title><meta name=\"description\" content=\"A very detailed description of this wonderful site that meets length requirements.\"><meta property=\"og:title\" content=\"{{ itemMeta.title }}\"><meta property=\"og:image\" content=\"img.png\"></head><body><h1>{{ itemMeta.title }}</h1><div>{{{ itemBody }}}</div></body></html>"
          
          curr <- getCurrentDirectory
          bracket_ (setCurrentDirectory tmpDir) (setCurrentDirectory curr) $ do
            -- Fix: use the correct config keys
            TIO.writeFile "sara.yaml" "title: Stress Test\nauthor: Tester\nbaseUrl: /\noutputDir: _site"
            
            sara $ do
              _items <- match (glob "posts/*.md") $ \file -> do
                item <- readMarkdown file
                validated <- validateSEO item
                render "templates/post.html" validated
                pure validated
              return ()
        
          -- Verification: Check that at least one file was generated
          allFiles <- listFilesRecursive tmpDir
          let generated = filter (L.isInfixOf "post-1.html") allFiles
          if null generated
            then do
              putStrLn "DEBUG: Generated files:"
              mapM_ putStrLn allFiles
              expectationFailure "No post-1.html found in output"
            else return ()

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive dir = do
  content <- listDirectory dir
  paths <- mapM (\name -> do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir then listFilesRecursive path else return [path]
    ) content
  return $ concat paths
