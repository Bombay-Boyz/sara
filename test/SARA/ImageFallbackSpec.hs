{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Regression test for a real "docstring promises one thing, code
--   does another" bug found in this session's error-handling audit:
--   'SARA.Asset.Image.processImage' documented itself as falling back
--   to copying the original file when a required conversion binary
--   (cwebp, avifenc, convert) is missing, and logging a warning — but
--   never actually checked binary availability or logged anything.
--   A missing binary would instead surface as Shake's own generic
--   subprocess-failure exception, naming neither the tool nor the
--   format.
--
--   This environment has 'convert' (ImageMagick) installed but not
--   'cwebp' or 'avifenc' — exactly the asymmetry needed to test both
--   branches for real, rather than mocking binary presence.
module SARA.ImageFallbackSpec (spec) where

import Test.Hspec
import Development.Shake
import SARA.Asset.Image (processImage)
import SARA.Types (ImageSpec(..), ImageFormat(..))
import SARA.Security.PathGuard (ProjectRoot(..), guardPath)
import SARA.Error (AnySaraError(..), renderAnyErrorColor)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import qualified System.Directory as Dir
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Data.IORef

-- | Run one 'processImage' call inside a minimal, real Shake session
--   (no rule registration needed — 'processImage' is a plain 'Action',
--   so a single top-level 'action' block is enough to execute it and
--   observe both its file-system effect and its returned issue list).
runProcessImage :: FilePath -> ImageSpec -> FilePath -> FilePath -> IO [AnySaraError]
runProcessImage root imgSpec input outBase = do
  resultRef <- newIORef []
  guardedInput <- guardPath (ProjectRoot root) input
  case guardedInput of
    Left err -> error ("test setup: guardPath rejected its own fixture: " ++ show err)
    Right safeInput ->
      shake shakeOptions { shakeFiles = root </> "_build", shakeVerbosity = Silent } $
        action $ do
          issues <- processImage imgSpec safeInput outBase
          liftIO $ writeIORef resultRef issues
  readIORef resultRef

spec :: Spec
spec = describe "SARA.Asset.Image.processImage — missing-binary fallback (not a silent failure)" $ do

  it "falls back to copying the original file when the required binary (cwebp) is missing, instead of crashing" $
    withSystemTempDirectory "sara-image-fallback" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "images")
      let input = tmpDir </> "images" </> "photo.png"
      -- Not a real PNG — processImage's fallback path is a plain file
      -- copy, so it never needs to decode the image at all.
      TIO.writeFile input "not-a-real-png-but-that-is-fine-for-a-copy"
      let output = tmpDir </> "_site" </> "images" </> "photo"
      createDirectoryIfMissing True (tmpDir </> "_site" </> "images")
      _ <- runProcessImage tmpDir (ImageSpec [] [WebP] 80) input output
      exists <- Dir.doesFileExist (output ++ "-original.webp")
      -- processImage names the fallback output the same way it would
      -- have named a real conversion (output -<.> suffix <.> ext); for
      -- a single-width spec, suffix is empty, so check the plain path.
      existsPlain <- Dir.doesFileExist (output ++ ".webp")
      (exists || existsPlain) `shouldBe` True

  it "reports which binary and format were missing, by name, rather than crashing or staying silent" $
    withSystemTempDirectory "sara-image-fallback-report" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "images")
      let input = tmpDir </> "images" </> "photo.png"
      TIO.writeFile input "not-a-real-png"
      let output = tmpDir </> "_site" </> "images" </> "photo"
      createDirectoryIfMissing True (tmpDir </> "_site" </> "images")
      issues <- runProcessImage tmpDir (ImageSpec [] [WebP] 80) input output
      length issues `shouldBe` 1
      let messages = map renderAnyErrorColor issues
      any (T.isInfixOf "cwebp") messages `shouldBe` True
      any (T.isInfixOf "WebP") messages `shouldBe` True

  it "reports no issues when the required binary (convert, for PNG) is actually present (control case)" $
    withSystemTempDirectory "sara-image-fallback-ok" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "images")
      let input = tmpDir </> "images" </> "photo.png"
      TIO.writeFile input "not-a-real-png"
      let output = tmpDir </> "_site" </> "images" </> "photo"
      createDirectoryIfMissing True (tmpDir </> "_site" </> "images")
      issues <- runProcessImage tmpDir (ImageSpec [] [PNG] 80) input output
      issues `shouldSatisfy` null
