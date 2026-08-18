{-# LANGUAGE OverloadedStrings #-}

module SARA.Asset.Placeholder
  ( generateLQIP
  ) where

import Codec.Picture
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.FilePath (takeExtension)
import Data.Char (toLower)

-- | Generates a 16x16 Base64-encoded blurred placeholder for an image.
--   Supports PNG and JPEG.
generateLQIP :: FilePath -> IO (Either String T.Text)
generateLQIP path = do
  let ext = map toLower (takeExtension path)
  bytes <- BS.readFile path
  let imgRes = case ext of
        ".png" -> decodePng bytes
        ".jpg" -> decodeJpeg bytes
        ".jpeg" -> decodeJpeg bytes
        _ -> Left "Unsupported format for LQIP"
  
  case imgRes of
    Left err -> return $ Left err
    Right dynamicImg -> do
      let rgb8 = convertRGB8 dynamicImg
      let small = scaleBilinear 16 16 rgb8
      let pngBytes = encodePng small
      let b64 = T.decodeUtf8 $ B64.encode (BSL.toStrict pngBytes)
      return $ Right $ "data:image/png;base64," <> b64

-- | Bilinear downscale to an exact @w x h@ target.
--
--   __Why this is here__: the codebase originally used
--   @Codec.Picture.Extra.scaleBilinear@ (the @JuicyPixels-extra@
--   package). That package is Hackage-only and this build environment
--   has no route to Hackage (see @sara.cabal@'s dependency-provenance
--   note), so this is a direct, minimal reimplementation of the one
--   function this module actually called from it — standard
--   source-space bilinear sampling: each destination pixel maps back
--   to a fractional source coordinate, and its value is a weighted
--   blend of the four nearest source pixels, edge-clamped so
--   coordinates just outside the source bounds still resolve.
scaleBilinear :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
scaleBilinear targetW targetH img = generateImage sample targetW targetH
  where
    srcW = imageWidth img
    srcH = imageHeight img

    -- Destination -> source coordinate mapping, sampling at pixel
    -- centers (the "+0.5 ... -0.5" pair) so a 1:1 resize is the
    -- identity map rather than being shifted by half a pixel.
    toSrcX tx = (fromIntegral tx + 0.5) * fromIntegral srcW / fromIntegral targetW - 0.5
    toSrcY ty = (fromIntegral ty + 0.5) * fromIntegral srcH / fromIntegral targetH - 0.5

    clampTo lo hi = max lo . min hi

    sample tx ty =
      let sx = toSrcX tx :: Double
          sy = toSrcY ty :: Double
          x0 = clampTo 0 (srcW - 1) (floor sx)
          y0 = clampTo 0 (srcH - 1) (floor sy)
          x1 = clampTo 0 (srcW - 1) (x0 + 1)
          y1 = clampTo 0 (srcH - 1) (y0 + 1)
          fx = sx - fromIntegral x0
          fy = sy - fromIntegral y0
          p00 = pixelAt img x0 y0
          p10 = pixelAt img x1 y0
          p01 = pixelAt img x0 y1
          p11 = pixelAt img x1 y1
      in lerpPixel fx fy p00 p10 p01 p11

    lerpPixel fx fy (PixelRGB8 r00 g00 b00) (PixelRGB8 r10 g10 b10)
                     (PixelRGB8 r01 g01 b01) (PixelRGB8 r11 g11 b11) =
      PixelRGB8 (chan r00 r10 r01 r11) (chan g00 g10 g01 g11) (chan b00 b10 b01 b11)
      where
        chan c00 c10 c01 c11 =
          let top    = lerp fx (fromIntegral c00) (fromIntegral c10)
              bottom = lerp fx (fromIntegral c01) (fromIntegral c11)
          in round (lerp fy top bottom :: Double)
        lerp t a b = a + t * (b - a)
