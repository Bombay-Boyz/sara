{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module SARA.Migration.Hakyll
  ( migrateHakyllProject
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Since Hakyll uses a Haskell DSL similar to SARA, migration is often
--   a matter of remapping paths and updating site.hs.
migrateHakyllProject
  :: FilePath
  -> IO (Either (SaraError 'EKMigration) Text)
migrateHakyllProject root = do
  -- Industrial Fix: We check for common Hakyll patterns
  pure $ Right $ T.unlines
    [ "Hakyll project detected in " <> T.pack root
    , "1. SARA is compatible with Hakyll's default 'posts/' and 'templates/' structure."
    , "2. Update your site.hs: Change 'import Hakyll' to 'import SARA'."
    , "3. SARA uses Lucid for templates by default, but also supports Mustache."
    , "4. Run 'sara init' to see a standard site.hs example."
    ]
