{-# LANGUAGE OverloadedStrings #-}

module SARA.Asset.BinaryCheck
  ( verifyBinaries
  ) where

import System.Directory (findExecutable)
import SARA.Error (SaraError(..), AnySaraError(..))
import qualified Data.Text as T

-- | Verifies that required binaries are in the PATH.
verifyBinaries :: [FilePath] -> IO (Either AnySaraError ())
verifyBinaries bins = do
  results <- mapM findExecutable bins
  let missing = [ b | (b, Nothing) <- zip bins results ]
  case missing of
    [] -> pure $ Right ()
    ms -> pure $ Left $ AnySaraError $ AssetProcessingFailed "System" 
            (T.pack $ "Missing required binaries: " ++ show ms)
