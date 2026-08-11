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
import SARA.Error (SaraBuildException(..))
import Control.Exception (throwIO)
import qualified Data.Text as T

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
    -- Shake's Oracle callback runs in 'Action', which (like any '%>'
    -- rule body) has no 'Either'-shaped failure channel of its own —
    -- see 'SaraBuildException's Haddock for why this throw, not a bare
    -- 'Prelude.error', is the standard's recorded, justified escape
    -- hatch for this specific boundary (5.9).
    Left err -> liftIO $ throwIO (SaraBuildException (T.pack ("LQIP Oracle failed for " ++ path ++ ": " ++ err)))

askLQIP :: FilePath -> Action Text
askLQIP path = askOracle (LQIPOracle path)
