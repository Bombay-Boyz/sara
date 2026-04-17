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
  , DataOracle(..)
  , addDataOracle
  , askData
  ) where

import Development.Shake
import Development.Shake.Classes
import GHC.Generics (Generic)
import Control.Monad (void)
import qualified BLAKE3
import qualified Data.ByteString as BS
import Data.Text (Text)
import qualified Data.Text as T
import SARA.Asset.Placeholder (generateLQIP)
import qualified Data.Aeson as Aeson
import qualified Data.Yaml as Yaml
import System.FilePath (takeExtension)

newtype BLAKE3Oracle = BLAKE3Oracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult BLAKE3Oracle = String

addBlake3Oracle :: Rules ()
addBlake3Oracle = void $ addOracle $ \(BLAKE3Oracle path) -> do
  need [path]
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
  need [path]
  res <- liftIO $ generateLQIP path
  case res of
    Right b64 -> return b64
    Left err -> fail $ "LQIP Oracle failed for " ++ path ++ ": " ++ err

askLQIP :: FilePath -> Action Text
askLQIP path = askOracle (LQIPOracle path)

newtype DataOracle = DataOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

-- | Wrapper to provide Binary instance for Aeson.Value
newtype BinaryValue = BinaryValue { unBinaryValue :: Aeson.Value }
  deriving (Show, Eq, Generic, NFData, Hashable)

instance Binary BinaryValue where
  put (BinaryValue v) = put (Aeson.encode v)
  get = do
    bs <- get
    case Aeson.decode bs of
      Just v -> return (BinaryValue v)
      Nothing -> fail "Failed to decode Aeson.Value in Binary instance"

type instance RuleResult DataOracle = BinaryValue

addDataOracle :: Rules ()
addDataOracle = void $ addOracle $ \(DataOracle path) -> do
  need [path]
  content <- liftIO $ BS.readFile path
  let ext = takeExtension path
  case ext of
    ".json" -> case Aeson.decodeStrict content of
                 Just v -> return (BinaryValue v)
                 Nothing -> fail $ "Data Oracle failed for " ++ path ++ ": Failed to parse JSON"
    ".yaml" -> case Yaml.decodeEither' content of
                 Right v -> return (BinaryValue v)
                 Left err -> fail $ "Data Oracle failed for " ++ path ++ ": " ++ show err
    _       -> fail $ "Data Oracle failed for " ++ path ++ ": Unsupported data format: " ++ ext

askData :: FilePath -> Action Aeson.Value
askData path = unBinaryValue <$> askOracle (DataOracle path)
