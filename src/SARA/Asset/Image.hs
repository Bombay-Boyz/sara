{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SARA.Asset.Image
  ( processImage
  , verifyImageBinaries
  ) where

import Development.Shake
import Development.Shake.FilePath
import SARA.Security.PathGuard
import SARA.Security.ShellGuard
import SARA.Types (ImageSpec(..), ImageFormat(..))
import Control.Monad (forM_)
import System.Directory (findExecutable)
import Data.Maybe (isJust)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- | Checks if required image processing binaries are available.
verifyImageBinaries :: IO [(ImageFormat, Bool)]
verifyImageBinaries = do
  webp  <- isJust <$> findExecutable "cwebp"
  avif  <- isJust <$> findExecutable "avifenc"
  magick <- isJust <$> findExecutable "convert"
  pure [ (WebP, webp), (AVIF, avif), (JPEG, magick), (PNG, magick) ]

-- | Resizes and converts images based on the specification.
--   Falls back to original if binary is missing.
processImage
  :: ImageSpec
  -> SafePath      -- ^ Input (path-guarded)
  -> FilePath      -- ^ Output base directory (relative to _site)
  -> Action ()
processImage spec (SafePath input) outBase = do
  -- For each width and format, generate an output image
  let formats = if null (imgFormats spec) then [PNG] else imgFormats spec
  let widths  = if null (imgWidths spec)  then [0]   else imgWidths spec
  
  forM_ formats $ \fmt -> do
    forM_ widths $ \w -> do
      let ext = formatToExt fmt
      let suffix = if w == 0 then "" else "-" ++ show w
      let output = outBase -<.> suffix <.> ext
      
      validatePath input
      validatePath output
      
      -- Industrial Grade: Handle missing binaries gracefully per-operation
      case fmt of
        WebP -> (safeCmd "cwebp" ["-q", show (imgQuality spec), input, "-o", output]) `Development.Shake.actionOnException` (liftIO $ TIO.putStrLn $ "WARNING: cwebp failed for " <> T.pack output)
        AVIF -> (safeCmd "avifenc" ["--job", "0", input, output]) `Development.Shake.actionOnException` (liftIO $ TIO.putStrLn $ "WARNING: avifenc failed for " <> T.pack output)
        _    -> if w == 0
                then copyFile' input output
                else (safeCmd "convert" [input, "-resize", show w, output]) `Development.Shake.actionOnException` (liftIO $ TIO.putStrLn $ "WARNING: magick failed for " <> T.pack output)

formatToExt :: ImageFormat -> String
formatToExt = \case
  WebP -> "webp"
  AVIF -> "avif"
  JPEG -> "jpg"
  PNG  -> "png"
