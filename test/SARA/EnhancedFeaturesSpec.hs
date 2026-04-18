{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.EnhancedFeaturesSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad (SaraEnv(..), SaraState(..), RuleDecl(..), SaraM(..), initialState)
import SARA.Security.PathGuard (mkProjectRoot)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.IORef
import Control.Monad.Reader (runReaderT)
import System.Directory (createDirectoryIfMissing, doesFileExist, getCurrentDirectory, setCurrentDirectory)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import SARA.Migration.Scaffold (scaffoldProject, ScaffoldOptions(..))
import qualified Control.Exception
import SARA.Config (defaultConfig, cfgOutputDirectory)

spec :: Spec
spec = do
  describe "Enhanced Industrial Features" $ do
    it "remaps metadata correctly across two phases" $ do
      withSystemTempDirectory "sara-remap-test" $ \tmpDir -> do
        let postsDir = tmpDir </> "posts"
        createDirectoryIfMissing True postsDir
        -- remapMetadata RENAMES a key, so the key must exist
        TIO.writeFile (postsDir </> "test.md") "---\nfromKey: someValue\n---\nBody"

        let config = defaultConfig { cfgOutputDirectory = tmpDir </> "_site" }
        root <- mkProjectRoot tmpDir
        stateRef <- newIORef initialState

        let dsl = do
              remapMetadata [("fromKey", "toKey")]
              void $ match (glob "posts/*.md") $ \file -> do
                item <- readMarkdown file
                validateSEO item

        -- Step 1: Pass 1 (Planning)
        let initialEnv = SaraEnv
              { envConfig     = config
              , envRoot       = root
              , envIsPlanning = True
              , envRemapRules = []
              , envState      = stateRef
              }

        resPass1 <- Control.Exception.try (runReaderT (unSaraM dsl) initialEnv) :: IO (Either Control.Exception.SomeException ())
        case resPass1 of
          Right () -> do
            state <- readIORef stateRef
            let rules = reverse (stateRules state)
            let allRemapRules = concat [ rs | RuleRemap rs <- rules ]
            allRemapRules `shouldContain` [("fromKey", "toKey")]

            -- Step 2: Pass 2 (Execution - simulate build behavior)
            let finalEnv = initialEnv { envRemapRules = allRemapRules, envIsPlanning = False }
            resPass2 <- Control.Exception.try (runReaderT (unSaraM (readMarkdown (T.pack $ postsDir </> "test.md"))) finalEnv) :: IO (Either Control.Exception.SomeException (Item 'Unvalidated))
            case resPass2 of
              Right item -> do
                KM.lookup "toKey" (itemMeta item) `shouldBe` Just (Aeson.String "someValue")
                KM.member "fromKey" (itemMeta item) `shouldBe` False
              Left e -> expectationFailure $ "Read markdown failed: " ++ show e
          Left e -> expectationFailure $ "Pass 1 failed: " ++ show e

    it "supports dynamic custom shortcode registration" $ do
      withSystemTempDirectory "sara-shortcode-reg" $ \tmpDir -> do
        let postsDir = tmpDir </> "posts"
        createDirectoryIfMissing True postsDir
        TIO.writeFile (postsDir </> "test.md") "---\ntitle: Test\ndescription: Test\n---\nHello {{% custom %}}"

        let config = defaultConfig { cfgOutputDirectory = tmpDir </> "_site" }
        root <- mkProjectRoot tmpDir
        stateRef <- newIORef initialState

        let dsl = do
              registerShortcode "custom" (\_ -> return "WORLD")
              match (glob "posts/*.md") $ \file -> do
                item <- readMarkdown file
                validateSEO item

        let env = SaraEnv config root False [] stateRef
        -- Run in execution mode directly to test expansion
        -- We must be in the right directory for 'match' (globDir1)
        curr <- System.Directory.getCurrentDirectory
        res <- Control.Exception.bracket_ 
                 (System.Directory.setCurrentDirectory tmpDir)
                 (System.Directory.setCurrentDirectory curr)
                 (Control.Exception.try (runReaderT (unSaraM dsl) env) :: IO (Either Control.Exception.SomeException [Item 'Validated]))
        case res of
          Right [item] -> T.isInfixOf "Hello WORLD" (itemBody item) `shouldBe` True
          Right _ -> expectationFailure "Expected exactly one item"
          Left e -> expectationFailure $ "DSL failed: " ++ show e

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
