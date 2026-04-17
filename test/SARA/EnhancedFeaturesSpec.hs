{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SARA.EnhancedFeaturesSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad
import SARA.Search.Index
import SARA.Migration.Scaffold
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import Data.IORef
import qualified Data.HashSet as HS
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified Data.Text.IO as TIO
import Control.Monad.Writer (runWriterT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Control.Monad (void)
import qualified BLAKE3
import qualified Data.ByteString as BS

spec :: Spec
spec = do
  describe "SARA Enhanced Features" $ do
    
    it "verifies Inverted Index generation logic" $ do
      let dummyHash = BLAKE3.hash Nothing ([] :: [BS.ByteString])
      let (e1, c1) = mkSearchEntry (Item "posts/1.md" (ResolvedRoute "url1") KM.empty "Content one" dummyHash)
      seUrl e1 `shouldBe` "url1"
      c1 `shouldBe` "Content one"

    it "verifies Global Metadata Remapping across the DSL passes" $ do
      withSystemTempDirectory "sara-metadata-test" $ \tmpDir -> do
        let postsDir = tmpDir </> "posts"
        createDirectoryIfMissing True postsDir
        -- remapMetadata RENAMES a key, so the key must exist
        TIO.writeFile (postsDir </> "test.md") "---\nfromKey: someValue\n---\nBody"
        
        graphRef <- newIORef HS.empty
        errorRef <- newIORef False
        let config = defaultConfig { cfgOutputDirectory = tmpDir </> "_site" }
        root <- mkProjectRoot tmpDir
        
        let dsl = do
              remapMetadata [("fromKey", "toKey")]
              void $ match (glob "posts/*.md") $ \file -> do
                item <- readMarkdown file
                validateSEO item

        -- Step 1: Pass 1
        let initialEnv = SaraEnv config root graphRef [] errorRef
        resPass1 <- runExceptT $ runWriterT $ runReaderT (unSaraM dsl) initialEnv
        case resPass1 of
          Right ((), rules) -> do
            let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
            allRemapRules `shouldContain` [("fromKey", "toKey")]
            
            -- Step 2: Pass 2 (simulate build behavior)
            let finalEnv = initialEnv { envRemapRules = allRemapRules }
            resPass2 <- runExceptT $ runWriterT $ runReaderT (unSaraM (readMarkdown (postsDir </> "test.md"))) finalEnv
            case resPass2 of
              Right (item, _) -> do
                KM.lookup "toKey" (itemMeta item) `shouldBe` Just (Aeson.String "someValue")
                KM.member "fromKey" (itemMeta item) `shouldBe` False
              Left e -> expectationFailure $ "Read markdown failed: " ++ show e
          Left e -> expectationFailure $ "Pass 1 failed: " ++ show e

    it "verifies the new Scaffold contains Wow features" $ do
      let opts = ScaffoldOptions "Test Site" "Author" "/"
      withSystemTempDirectory "sara-scaffold-wow" $ \tmpDir -> do
        scaffoldProject tmpDir opts
        
        -- Check for search.js
        exists <- doesFileExist (tmpDir </> "assets" </> "search.js")
        exists `shouldBe` True
        
        -- Check for View Transitions in template
        tplContent <- TIO.readFile (tmpDir </> "templates" </> "post.html")
        T.isInfixOf "view-transition" tplContent `shouldBe` True
        
        -- Check for Dark Mode CSS
        T.isInfixOf "prefers-color-scheme: dark" tplContent `shouldBe` True
