{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the quality-seal file bridge added to close the
--   "live-reload doesn't work for a subprocess-executed site.hs"
--   limitation. The actual browser hot-reload
--   ('SARA.LiveReload.Server'\/@broadcastPatches@ in app/Main.hs)
--   never needed this — it reads the built '_site' directory from
--   disk after the build completes, which works identically whether
--   the build ran in-process or via a subprocess. The one real gap
--   was the secondary "quality seal" dashboard message, broadcast
--   directly from inside 'SARA.saraWithOptions', which a subprocess
--   has no way to do since it doesn't hold the parent process's
--   'MVar'. This spec checks the file-based bridge that fixes that:
--   'SARA.saraWithOptions' always persists the report, regardless of
--   whether it could also broadcast it directly.
module SARA.QualitySealBridgeSpec (spec) where

import Test.Hspec
import SARA
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Exception (finally)
import qualified Data.Aeson as Aeson
import Data.Aeson ((.:))
import Data.Aeson.Types (parseMaybe)

withOnePostProject :: (FilePath -> IO a) -> IO a
withOnePostProject action =
  withSystemTempDirectory "sara-qseal-bridge" $ \tmpDir -> do
    oldCwd <- getCurrentDirectory
    (`finally` setCurrentDirectory oldCwd) $ do
      createDirectoryIfMissing True (tmpDir </> "posts")
      createDirectoryIfMissing True (tmpDir </> "templates")
      TIO.writeFile (tmpDir </> "posts" </> "hello.md")
        "---\ntitle: Hello\nauthor: Tester\n---\nBody."
      TIO.writeFile (tmpDir </> "templates" </> "post.html")
        "<html><head><title>{{title}}</title></head><body>{{{itemBody}}}</body></html>"
      setCurrentDirectory tmpDir
      action tmpDir

spec :: Spec
spec = describe "Quality-seal file bridge (subprocess site.hs live-reload dashboard)" $ do

  it "writes .sara/quality-seal.json on every real (non-dry-run) build" $
    withOnePostProject $ \tmpDir -> do
      sara $ do
        item <- readMarkdown "posts/hello.md"
        item' <- validateSEO item
        render "templates/post.html" item'
      exists <- doesFileExist (tmpDir </> ".sara" </> "quality-seal.json")
      exists `shouldBe` True

  it "readQualitySealFile reads back a value matching the real build (item count, security status)" $
    withOnePostProject $ \_ -> do
      sara $ do
        item <- readMarkdown "posts/hello.md"
        item' <- validateSEO item
        render "templates/post.html" item'
      mSeal <- readQualitySealFile
      case mSeal of
        Nothing -> expectationFailure "Expected a quality-seal file to be readable after a real build"
        Just sealValue -> do
          let itemCount = flip parseMaybe sealValue $ \o -> case o of
                Aeson.Object obj -> obj .: "qsItemCount"
                _ -> fail "not an object"
          itemCount `shouldBe` Just (1 :: Int)

  it "readQualitySealFile returns Nothing gracefully when no report exists yet" $
    withSystemTempDirectory "sara-qseal-bridge-none" $ \tmpDir -> do
      oldCwd <- getCurrentDirectory
      (`finally` setCurrentDirectory oldCwd) $ do
        setCurrentDirectory tmpDir
        mSeal <- readQualitySealFile
        mSeal `shouldBe` Nothing
