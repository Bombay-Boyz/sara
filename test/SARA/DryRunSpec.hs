{-# LANGUAGE OverloadedStrings #-}

-- | Regression test for a previously-dead CLI flag: 'app/Main.hs' parsed
--   '--dry-run' into 'BuildOpts' but discarded it
--   (@runDefaultBuild mClients _@), and 'SARA.saraWithClients' hardcoded
--   'cfgDryRun = False' regardless of what was passed in. Per 1.11 of the
--   Haskell Engineering Standard ("if a capability isn't ready, its call
--   site isn't merged either"), a flag that's parsed and silently
--   dropped is worse than no flag at all — this spec exists to make sure
--   that can't regress silently again: it exercises 'saraWithOptions'
--   directly and checks the one property that actually matters for a
--   dry run — that the output directory receives zero files.
module SARA.DryRunSpec (spec) where

import Test.Hspec
import SARA
import System.IO.Temp (withSystemTempDirectory)
import System.Directory
  ( createDirectoryIfMissing
  , setCurrentDirectory
  , getCurrentDirectory
  , doesDirectoryExist
  , listDirectory
  )
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.List (isInfixOf)
import System.IO (stdout, hFlush, hClose, hSetBuffering, BufferMode(..), openTempFile)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Control.Monad (void)
import Control.Exception (finally)
import System.Directory (removeFile)

-- | Shared fixture: a tiny one-post project, written into a fresh temp
--   directory, with the working directory switched there for the
--   duration of the action and restored afterwards regardless of
--   success or failure.
withOnePostProject :: (FilePath -> IO a) -> IO a
withOnePostProject action =
  withSystemTempDirectory "sara-dryrun" $ \tmpDir -> do
    oldCwd <- getCurrentDirectory
    (`finally` setCurrentDirectory oldCwd) $ do
      createDirectoryIfMissing True (tmpDir </> "posts")
      createDirectoryIfMissing True (tmpDir </> "templates")
      TIO.writeFile (tmpDir </> "posts" </> "hello.md")
        (T.unlines ["---", "title: Hello", "author: Tester", "---", "# Hi"])
      TIO.writeFile (tmpDir </> "templates" </> "post.html")
        "<html><head><title>{{title}}</title></head><body>{{{itemBody}}}</body></html>"
      setCurrentDirectory tmpDir
      action tmpDir

runOnePostBuild :: Bool -> IO ()
runOnePostBuild dryRun =
  saraWithOptions Nothing dryRun $ do
    postsGlob <- glob "posts/*.md"
    void $ match postsGlob $ \file -> do
      item <- readMarkdown file
      item' <- validateSEO item
      render "templates/post.html" item'
      pure item'

-- | Minimal stdout capture: redirect fd1 to a temp file for the
--   duration of the action, then read it back. Avoids pulling in an
--   extra dependency (e.g. 'silently') for one test.
captureStdout :: IO a -> IO String
captureStdout action = do
  (tmpPath, tmpHandle) <- openTempFile "." "sara-dryrun-capture.txt"
  hSetBuffering tmpHandle NoBuffering
  savedStdout <- hDuplicate stdout
  hFlush stdout
  hDuplicateTo tmpHandle stdout
  _ <- action `finally` (hFlush stdout >> hDuplicateTo savedStdout stdout >> hClose savedStdout)
  hClose tmpHandle
  contents <- readFile tmpPath
  length contents `seq` removeFile tmpPath
  pure contents

spec :: Spec
spec = describe "SARA --dry-run (SaraConfig.cfgDryRun via saraWithOptions)" $ do

  it "writes zero files under _site when dryRun = True" $
    withOnePostProject $ \tmpDir -> do
      runOnePostBuild True
      let siteDir = tmpDir </> "_site"
      exists <- doesDirectoryExist siteDir
      if exists
        then do
          contents <- listAllFiles siteDir
          contents `shouldBe` []
        else pure () -- never creating the directory at all is also correct for a dry run

  it "still reports the post's planned output path on a dry run (RuleMatch is expanded, not just the literal top-level decl)" $
    withOnePostProject $ \_ -> do
      output <- captureStdout (runOnePostBuild True)
      output `shouldSatisfy` isInfixOf "hello.html"
      output `shouldSatisfy` isInfixOf "1 file(s) would be written"

  it "writes at least one file under _site when dryRun = False (control case)" $
    withOnePostProject $ \tmpDir -> do
      runOnePostBuild False
      let siteDir = tmpDir </> "_site"
      exists <- doesDirectoryExist siteDir
      exists `shouldBe` True
      contents <- listAllFiles siteDir
      contents `shouldNotBe` []

listAllFiles :: FilePath -> IO [FilePath]
listAllFiles dir = do
  names <- listDirectory dir
  fmap concat . mapM go $ names
  where
    go name = do
      let path = dir </> name
      isDir <- doesDirectoryExist path
      if isDir then listAllFiles path else pure [path]
