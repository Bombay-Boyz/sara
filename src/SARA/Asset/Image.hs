{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module SARA.Asset.Image
  ( processImage
  , verifyImageBinaries
  ) where

import Development.Shake
import Development.Shake.FilePath
import SARA.Security.PathGuard
import SARA.Security.ShellGuard
import SARA.Types (ImageSpec(..), ImageFormat(..))
import SARA.Error (SaraError(..), AnySaraError(..))
import Control.Monad (forM)
import System.Directory (findExecutable)
import Data.Maybe (isJust)
import qualified Data.Text as T

-- | Checks if required image processing binaries are available.
verifyImageBinaries :: IO [(ImageFormat, Bool)]
verifyImageBinaries = do
  webp  <- isJust <$> findExecutable "cwebp"
  avif  <- isJust <$> findExecutable "avifenc"
  magick <- isJust <$> findExecutable "convert"
  pure [ (WebP, webp), (AVIF, avif), (JPEG, magick), (PNG, magick) ]

-- | Binary each format's conversion actually shells out to, so a
--   missing-tool message can name the specific thing to install
--   instead of "image processing failed."
binaryFor :: ImageFormat -> FilePath
binaryFor WebP = "cwebp"
binaryFor AVIF = "avifenc"
binaryFor JPEG = "convert"
binaryFor PNG  = "convert"

-- | Resizes and converts images based on the specification.
--
--   Genuinely falls back to copying the original file when a required
--   binary is missing, and reports exactly which binary and format
--   were affected — rather than the previous behaviour, which
--   despite an identically-worded docstring ("Falls back to original
--   if binary is missing... we check and log warnings") never actually
--   called 'verifyImageBinaries' or logged anything at all, and would
--   instead let Shake's own generic subprocess-failure exception crash
--   the whole build with a message that never named the missing tool.
--   A promise in a docstring that the implementation doesn't keep is
--   its own kind of silent failure — the code silently diverges from
--   its own contract.
processImage
  :: ImageSpec
  -> SafePath      -- ^ Input (path-guarded)
  -> FilePath      -- ^ Output base directory (relative to _site)
  -> Action [AnySaraError]
processImage spec input outBase = do
  let inputPath = unSafePath input
  let formats = if null (imgFormats spec) then [PNG] else imgFormats spec
  let widths  = if null (imgWidths spec)  then [0]   else imgWidths spec
  available <- liftIO verifyImageBinaries

  fmap concat . forM formats $ \fmt ->
    fmap concat . forM widths $ \w -> do
      let ext = formatToExt fmt
      let suffix = if w == 0 then "" else "-" ++ show w
      let output = outBase -<.> suffix <.> ext
      let binaryPresent = maybe True id (lookup fmt available)

      if not binaryPresent
        then do
          -- Fall back to copying the original, exactly as documented,
          -- and say so — silently succeeding with a lower-quality (or
          -- unconverted) asset and no record of why would be its own
          -- silent failure, just one that "worked."
          copyFile' inputPath output
          pure [ AnySaraError $ AssetProcessingFailed inputPath
                   ( "required binary '" <> T.pack (binaryFor fmt)
                   <> "' for " <> T.pack (show fmt)
                   <> " conversion was not found on PATH; copied the original file to "
                   <> T.pack output <> " unconverted instead" )
               ]
        else case fmt of
          WebP -> safeCmd "cwebp" ["-q", show (imgQuality spec), inputPath, "-o", output] >> pure []
          AVIF -> safeCmd "avifenc" ["--job", "0", inputPath, output] >> pure []
          _    -> if w == 0
                  then copyFile' inputPath output >> pure []
                  else safeCmd "convert" [inputPath, "-resize", show w, output] >> pure []

formatToExt :: ImageFormat -> String
formatToExt = \case
  WebP -> "webp"
  AVIF -> "avif"
  JPEG -> "jpg"
  PNG  -> "png"
