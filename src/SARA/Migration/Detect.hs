{-# LANGUAGE OverloadedStrings #-}

module SARA.Migration.Detect
  ( SourceSSG(..)
  , detectSourceSSG
  ) where

import System.Directory (doesFileExist, doesDirectoryExist)
import System.FilePath ((</>))

data SourceSSG
  = SourceJekyll
  | SourceHugo
  | SourceHakyll
  | SourceUnknown
  deriving (Eq, Show)

-- | Heuristics to detect the source SSG.
detectSourceSSG :: FilePath -> IO SourceSSG
detectSourceSSG root = do
  isJekyll <- checkJekyll root
  if isJekyll then pure SourceJekyll else do
    isHugo <- checkHugo root
    if isHugo then pure SourceHugo else do
      isHakyll <- checkHakyll root
      if isHakyll then pure SourceHakyll else pure SourceUnknown

checkJekyll :: FilePath -> IO Bool
checkJekyll root = do
  hasConfig <- doesFileExist (root </> "_config.yml")
  hasPosts  <- doesDirectoryExist (root </> "_posts")
  pure (hasConfig && hasPosts)

checkHugo :: FilePath -> IO Bool
checkHugo root = do
  hasToml <- doesFileExist (root </> "config.toml")
  hasYaml <- doesFileExist (root </> "config.yaml")
  hasContent <- doesDirectoryExist (root </> "content")
  pure ((hasToml || hasYaml) && hasContent)

checkHakyll :: FilePath -> IO Bool
checkHakyll root = do
  hasSiteHs <- doesFileExist (root </> "site.hs")
  hasSiteCabal <- doesFileExist (root </> "site.cabal")
  pure (hasSiteHs || hasSiteCabal)
