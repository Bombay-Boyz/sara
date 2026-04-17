{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.EnhancedFeaturesSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad (SaraEnv(..), RuleDecl(..), SaraM(..))
import SARA.Config (defaultConfig, cfgOutputDirectory)
import SARA.Security.PathGuard (mkProjectRoot)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import SARA.Migration.Scaffold (scaffoldProject, ScaffoldOptions(..))

spec :: Spec
spec = do
  describe "Enhanced Industrial Features" $ do
    it "remaps metadata correctly across two phases" $ do
      withSystemTempDirectory "sara-remap-test" $ \tmpDir -> do
        let postsDir = tmpDir </> "posts"
        createDirectoryIfMissing True postsDir
        -- remapMetadata RENAMES a key, so the key must exist
        TIO.writeFile (postsDir </> "test.md") "---\nfromKey: someValue\n---\nBody"
        
        graphRef <- newIORef HS.empty
        errorRef <- newIORef False
        rulesRef <- newIORef []
        itemCacheRef <- newIORef Map.empty
        dataCacheRef <- newIORef Map.empty
        
        let config = defaultConfig { cfgOutputDirectory = tmpDir </> "_site" }
        root <- mkProjectRoot tmpDir
        
        let dsl = do
              remapMetadata [("fromKey", "toKey")]
              void $ match (glob "posts/*.md") $ \file -> do
                item <- readMarkdown file
                validateSEO item

        -- Step 1: Pass 1 (Planning)
        let initialEnv = SaraEnv config root graphRef [] errorRef rulesRef True itemCacheRef dataCacheRef
        resPass1 <- runExceptT $ runReaderT (unSaraM dsl) initialEnv
        case resPass1 of
          Right () -> do
            rules <- readIORef rulesRef
            let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
            allRemapRules `shouldContain` [("fromKey", "toKey")]
            
            -- Step 2: Pass 2 (Execution - simulate build behavior)
            let finalEnv = initialEnv { envRemapRules = allRemapRules, envIsPlanning = False }
            resPass2 <- runExceptT $ runReaderT (unSaraM (readMarkdown (postsDir </> "test.md"))) finalEnv
            case resPass2 of
              Right item -> do
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
