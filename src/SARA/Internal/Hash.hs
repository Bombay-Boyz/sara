{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module SARA.Internal.Hash
  ( BLAKE3Oracle
  , addBlake3Oracle
  , needBlake3
  , LQIPOracle
  , addLQIPOracle
  , askLQIP
  ) where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Control.Monad (void)
import qualified BLAKE3
import qualified Data.ByteString as BS
import Data.Text (Text)
import SARA.Asset.Placeholder (generateLQIP)

newtype BLAKE3Oracle = BLAKE3Oracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult BLAKE3Oracle = String

addBlake3Oracle :: Rules ()
addBlake3Oracle = void $ addOracle $ \(BLAKE3Oracle path) ->
  liftIO $ show . (BLAKE3.hash Nothing :: [BS.ByteString] -> BLAKE3.Digest 32) . (:[]) <$> BS.readFile path

needBlake3 :: [FilePath] -> Action ()
needBlake3 paths = do
  _ <- askOracles (map BLAKE3Oracle paths)
  pure ()

newtype LQIPOracle = LQIPOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult LQIPOracle = Text

addLQIPOracle :: Rules ()
addLQIPOracle = void $ addOracle $ \(LQIPOracle path) -> do
  res <- liftIO $ generateLQIP path
  case res of
    Right b64 -> return b64
    Left err -> error $ "LQIP Oracle failed for " ++ path ++ ": " ++ err

askLQIP :: FilePath -> Action Text
askLQIP path = askOracle (LQIPOracle path)
