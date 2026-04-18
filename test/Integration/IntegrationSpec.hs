{-# LANGUAGE OverloadedStrings #-}

module Integration.IntegrationSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad (SPath)
import SARA.Config (defaultConfig, cfgOutputDirectory)
import SARA.Security.PathGuard (mkProjectRoot)
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)

spec :: Spec
spec = do
  describe "SARA Integration: Full Pipeline Test" $ do
    it "executes all phases from parsing to search indexing" $ do
      withSystemTempDirectory "sara-integration" $ \tmpDir -> do
        -- Setup
        let postsDir = tmpDir </> "posts"
        let tplDir = tmpDir </> "templates"
        createDirectoryIfMissing True postsDir
        createDirectoryIfMissing True tplDir
        
        TIO.writeFile (postsDir </> "hello.md") "---\ntitle: Hello\ndescription: Test\n---\n# World"
        TIO.writeFile (tplDir </> "post.html") "<html><body>{{{itemBody}}}</body></html>"
        
        let config = defaultConfig { cfgOutputDirectory = tmpDir </> "_site" }
        root <- mkProjectRoot tmpDir

        saraWithClients Nothing $ do
          posts <- match (glob "posts/*.md") $ \file -> do
            item <- readMarkdown file
            _ <- validateSEO item
            render "templates/post.html" (coerceToValidated item)
            pure item
          
          buildSearchIndex "search.json" posts

        -- Verify
        -- Note: outDir is tmpDir </> "_site", so full path is tmpDir </> "_site" </> "posts" </> "hello.html"
        doesFileExist (tmpDir </> "_site" </> "posts" </> "hello.html") `shouldReturn` True
        doesFileExist (tmpDir </> "_site" </> "search.json") `shouldReturn` True
