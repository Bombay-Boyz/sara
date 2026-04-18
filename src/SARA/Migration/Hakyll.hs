{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module SARA.Migration.Hakyll
  ( migrateHakyllProject
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import Data.Text (Text)

-- | Since Hakyll uses a Haskell DSL similar to SARA, migration is often
--   a matter of remapping paths and updating site.hs.
migrateHakyllProject
  :: FilePath
  -> IO (Either (SaraError 'EKMigration) Text)
migrateHakyllProject _ = do
  -- Implementation for Hakyll detection is in SARA.Migration.Detect.
  -- This function would perform more complex file moves if needed.
  pure $ Right "Hakyll project structure is compatible with SARA. Update your site.hs to use SARA imports."
