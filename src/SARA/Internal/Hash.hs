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
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import SARA.Asset.Placeholder (generateLQIP)
import SARA.Error (SaraBuildException(..))
import Control.Exception (throwIO)
import qualified Data.Text as T

-- | Historically named for the BLAKE3-based cache-key oracle this
--   replaced. This is a build-cache content key, not a security
--   boundary (nothing here is verified against an adversary — see
--   'SARA.Markdown.Parser' and 'SARA.Security.*' for the modules that
--   actually carry that responsibility), so SHA-256 — available from
--   this environment's apt archive, unlike the original 'blake3'
--   package, which Hackage-only distribution makes unreachable here
--   (see @sara.cabal@'s dependency-provenance note) — is equally fit
--   for purpose: a fast, collision-resistant digest used purely to
--   detect "this file's content changed since the last build."
newtype BLAKE3Oracle = BLAKE3Oracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult BLAKE3Oracle = String

addBlake3Oracle :: Rules ()
addBlake3Oracle = void $ addOracle $ \(BLAKE3Oracle path) ->
  liftIO $ T.unpack . T.decodeUtf8 . Base16.encode . SHA256.hash <$> BS.readFile path

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
