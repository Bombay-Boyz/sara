{-# LANGUAGE OverloadedStrings #-}

module SARA.Migration.Engine
  ( runMigration
  ) where

import SARA.Migration.Detect
import SARA.Migration.Hugo
import SARA.Migration.Jekyll
import SARA.Migration.Hakyll
import SARA.Migration.Scaffold (scaffoldProject, ScaffoldOptions(..))
import SARA.Error (AnySaraError(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (forM_, when)

-- | Run the migration engine on a source directory.
runMigration :: FilePath -> FilePath -> IO (Either AnySaraError Text)
runMigration src dst = do
  sourceType <- detectSourceSSG src
  case sourceType of
    SourceHugo   -> migrateHugo src dst
    SourceJekyll -> migrateJekyll src dst
    SourceHakyll -> migrateHakyll src dst
    SourceUnknown -> return $ Right "Unknown source SSG. No automated migration possible."

migrateHugo :: FilePath -> FilePath -> IO (Either AnySaraError Text)
migrateHugo src dst = do
  TIO.putStrLn "Migrating Hugo project..."
  createDirectoryIfMissing True (dst </> "posts")
  createDirectoryIfMissing True (dst </> "assets")
  
  -- 1. Move content to posts
  contentExists <- doesDirectoryExist (src </> "content")
  when contentExists $ copyAndTranslate src (src </> "content") (dst </> "posts") translateHugoShortcodes
  
  -- 2. Move static to assets
  staticExists <- doesDirectoryExist (src </> "static")
  when staticExists $ copyRecursive (src </> "static") (dst </> "assets")

  -- 3. Initial Scaffold for site.hs
  scaffoldProject dst (ScaffoldOptions "Migrated Hugo Site" "SARA User" "http://localhost:8080")
  
  return $ Right "Hugo migration complete. Check 'posts/' and 'assets/'."

migrateJekyll :: FilePath -> FilePath -> IO (Either AnySaraError Text)
migrateJekyll src dst = do
  TIO.putStrLn "Migrating Jekyll project..."
  createDirectoryIfMissing True (dst </> "posts")
  createDirectoryIfMissing True (dst </> "assets")
  
  -- 1. Move _posts to posts
  postsExists <- doesDirectoryExist (src </> "_posts")
  when postsExists $ copyAndTranslate src (src </> "_posts") (dst </> "posts") translateJekyllShortcodes
  
  -- 2. Move assets
  assetsExists <- doesDirectoryExist (src </> "assets")
  when assetsExists $ copyRecursive (src </> "assets") (dst </> "assets")

  scaffoldProject dst (ScaffoldOptions "Migrated Jekyll Site" "SARA User" "http://localhost:8080")
  
  return $ Right "Jekyll migration complete. Check 'posts/' and 'assets/'."

migrateHakyll :: FilePath -> FilePath -> IO (Either AnySaraError Text)
migrateHakyll src dst = do
  res <- migrateHakyllProject src
  case res of
    Left err -> return $ Left (AnySaraError err)
    Right msg -> do
      -- Just scaffold a base site in dst if they want to move over
      scaffoldProject dst (ScaffoldOptions "Migrated Hakyll Site" "SARA User" "http://localhost:8080")
      return $ Right msg

-- Internal Helpers

copyAndTranslate :: FilePath -> FilePath -> FilePath -> (FilePath -> T.Text -> Either e T.Text) -> IO ()
copyAndTranslate root src dst translate = do
  content <- listDirectory src
  forM_ content $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDir <- doesDirectoryExist srcPath
    if isDir
      then createDirectoryIfMissing True dstPath >> copyAndTranslate root srcPath dstPath translate
      else if takeExtension name == ".md"
           then do
             text <- TIO.readFile srcPath
             case translate srcPath text of
               Left _ -> do
                 TIO.putStrLn $ "WARNING: Translation failed for " <> T.pack srcPath <> ". Copying raw."
                 copyFile srcPath dstPath
               Right translated -> TIO.writeFile dstPath translated
           else copyFile srcPath dstPath

copyRecursive :: FilePath -> FilePath -> IO ()
copyRecursive src dst = do
  createDirectoryIfMissing True dst
  content <- listDirectory src
  forM_ content $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDir <- doesDirectoryExist srcPath
    if isDir
      then copyRecursive srcPath dstPath
      else copyFile srcPath dstPath
