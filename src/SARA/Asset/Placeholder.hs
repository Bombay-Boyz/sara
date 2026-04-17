{-# LANGUAGE OverloadedStrings #-}

module SARA.Asset.Placeholder
  ( generateLQIP
  ) where

import Codec.Picture
import Codec.Picture.Extra (scaleBilinear)
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
