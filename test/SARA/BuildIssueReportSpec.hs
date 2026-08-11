{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end test for the structured build-issue report introduced
--   when 'envHasErrors' (a bare 'IORef' 'Bool') became 'envBuildIssues'
--   (an 'IORef' '[BuildIssue]'). The two properties that matter, each
--   isolated in its own test:
--
--     1. The build's pass\/fail outcome is unchanged — any issue still
--        fails the build, exactly as a nonzero 'envHasErrors' did
--        before. This is deliberate: the *reporting* changed, the
--        *policy* (which issues fail a build) did not — see the
--        discussion in this session on why the severity question
--        (error vs. warning) is deferred, not resolved, by this change.
--     2. The printed report actually surfaces which *file* has the
--        problem and what the problem is, grouped, rather than a
--        single "something failed" line — the entire point of the
--        change.
module SARA.BuildIssueReportSpec (spec) where

import Test.Hspec
import SARA
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T
import Control.Monad (void)
import Control.Exception (finally, try, SomeException, fromException)
import System.Exit (ExitCode(..))
import System.IO (stdout, hFlush, hClose, hSetBuffering, BufferMode(..), openTempFile)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import System.Directory (removeFile)
import Data.List (isInfixOf)

-- | Same minimal stdout-capture technique as 'SARA.DryRunSpec' — kept
--   local rather than shared, since sharing test helpers across spec
--   modules isn't worth the coupling for one small function.
captureStdout :: IO a -> IO String
captureStdout action = do
  (tmpPath, tmpHandle) <- openTempFile "." "sara-issue-report-capture.txt"
  hSetBuffering tmpHandle NoBuffering
  savedStdout <- hDuplicate stdout
  hFlush stdout
  hDuplicateTo tmpHandle stdout
  _ <- action `finally` (hFlush stdout >> hDuplicateTo savedStdout stdout >> hClose savedStdout)
  hClose tmpHandle
  contents <- readFile tmpPath
  length contents `seq` removeFile tmpPath
  pure contents

-- | A two-post project: one clean, one with a hardcoded broken
--   internal link, so the build has exactly one attributable issue.
withBrokenLinkProject :: (FilePath -> IO a) -> IO a
withBrokenLinkProject action =
  withSystemTempDirectory "sara-issue-report" $ \tmpDir -> do
    oldCwd <- getCurrentDirectory
    (`finally` setCurrentDirectory oldCwd) $ do
      createDirectoryIfMissing True (tmpDir </> "posts")
      createDirectoryIfMissing True (tmpDir </> "templates")
      TIO.writeFile (tmpDir </> "posts" </> "clean.md")
        (T.unlines ["---", "title: Clean Post", "author: Tester", "---", "All good here."])
      TIO.writeFile (tmpDir </> "posts" </> "broken.md")
        (T.unlines ["---", "title: Broken Post", "author: Tester", "---", "See link."])
      -- A template with a hardcoded broken link, so every post rendered
      -- through it has exactly one findable, attributable issue.
      TIO.writeFile (tmpDir </> "templates" </> "post.html")
        "<html><head><title>{{title}}</title></head><body>{{{itemBody}}}<a href=\"does-not-exist.html\">missing</a></body></html>"
      setCurrentDirectory tmpDir
      action tmpDir

runTwoPostBuild :: IO ()
runTwoPostBuild =
  sara $
    void $ match (glob "posts/*.md") $ \file -> do
      item <- readMarkdown file
      item' <- validateSEO item
      render "templates/post.html" item'
      pure item'

spec :: Spec
spec = describe "Structured build-issue report (envBuildIssues)" $ do

  it "still fails the build when an issue is found (pass/fail policy unchanged)" $
    withBrokenLinkProject $ \_ -> do
      result <- try runTwoPostBuild :: IO (Either SomeException ())
      case result of
        Left e -> case fromException e of
          Just (ExitFailure _) -> pure () -- correct: build must still fail
          Just ExitSuccess      -> expectationFailure "Build reported success despite a broken link"
          Nothing               -> expectationFailure ("Build failed with an unexpected exception: " ++ show e)
        Right () -> expectationFailure "Build should have exited with failure due to the broken link"

  it "reports the broken link attributed to its actual output file, not just a pass/fail flag" $
    withBrokenLinkProject $ \_ -> do
      output <- captureStdout (try runTwoPostBuild :: IO (Either SomeException ()))
      -- The report must name the specific file the issue came from...
      -- (output path, now .html per the SlugRoute default fix)
      output `shouldSatisfy` isInfixOf "broken.html"
      -- ...and the actual problem, not just an aggregate signal.
      output `shouldSatisfy` isInfixOf "does-not-exist.html"
      -- The old presentation (bare pass/fail with no file attribution)
      -- must be gone: a build with issues doesn't claim IRONCLAD.
      output `shouldNotSatisfy` isInfixOf "IRONCLAD"
