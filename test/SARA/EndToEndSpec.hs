module SARA.EndToEndSpec (spec) where

import Test.Hspec
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, setCurrentDirectory, getCurrentDirectory, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (finally)
import Data.List (isInfixOf)

-- | Resolves the already-built 'sara' executable's real path via
--   @cabal list-bin sara@, run from the SARA source tree.
--
--   This replaced an earlier version of this test that invoked
--   @cabal run --project-dir \<sara-source\> sara -- build@ from the
--   temp project directory instead. That relied on '--project-dir'
--   being a flag 'cabal run' accepts -- true from cabal-install roughly
--   3.10\/3.12 onward (confirmed via that version's docs), but this
--   very sandbox runs cabal-install 3.8.1.0, whose own
--   @cabal run --help@ output, checked directly while diagnosing this,
--   lists '--project-file' but not '--project-dir' at all for the
--   'run' subcommand. That's not a quirk unique to this sandbox: SARA
--   itself claims support back to GHC 9.4.7 (see @tested-with@ in
--   sara.cabal), and GHC 9.4.7 predates cabal-install 3.12 -- a real
--   user pairing an older GHC with the cabal-install that shipped
--   around the same time would hit the exact same failure, in their
--   own environment, with no SARA bug to blame. 'cabal list-bin' has
--   been stable since long before either cabal version and needs no
--   equivalent flag at all: resolve the binary path once, from the
--   source tree, then invoke that path directly -- no '--project-dir'
--   equivalent needed for the rest of the test.
resolveSaraBinary :: FilePath -> IO FilePath
resolveSaraBinary saraSourceDir = do
  (exitCode, out, err) <- readProcessWithExitCode "cabal" ["list-bin", "sara"] ""
  case exitCode of
    ExitSuccess -> pure (takeWhile (`notElem` ("\r\n" :: String)) out)
    _ -> error $ "cabal list-bin sara failed (run from " ++ saraSourceDir ++ "): " ++ out ++ err

spec :: Spec
spec = do
  describe "SARA End-to-End: CLI Execution" $ do
    it "runs 'sara build' and generates a real, correctly-rendered site file" $ do
      oldDir <- getCurrentDirectory
      -- Resolved before changing directory: 'cabal list-bin' needs to
      -- run from the SARA source tree, where its own cabal.project
      -- lives, not from the temp test project we're about to create.
      saraBinary <- resolveSaraBinary oldDir
      binaryExists <- doesFileExist saraBinary
      binaryExists `shouldBe` True

      withSystemTempDirectory "sara-e2e" $ \tmpDir -> do
        (`finally` setCurrentDirectory oldDir) $ do
          -- Setup Project
          createDirectory (tmpDir </> "posts")
          createDirectory (tmpDir </> "templates")
          TIO.writeFile (tmpDir </> "posts" </> "test.md") 
            (T.pack "---\ntitle: E2E Industrial Test\nauthor: E2E Tester\n---\n# Hello\nWelcome to SARA.")
          TIO.writeFile (tmpDir </> "templates" </> "post.html") 
            (T.pack "<html><head><title>{{title}}</title></head><body><h1>{{title}}</h1></body></html>")
          
          setCurrentDirectory tmpDir
          (exitCode, out, err) <- readProcessWithExitCode saraBinary ["build"] ""
          
          if exitCode /= ExitSuccess 
            then expectationFailure $ "Build failed: " ++ out ++ "\n" ++ err
            else do
              -- Not just the exit code: the real output file must
              -- exist and actually contain the post's real title --
              -- this session's own audit found more than one bug that
              -- an exit-code-only check like the original version of
              -- this test would never have caught (e.g. a file
              -- silently never being written at all).
              rendered <- doesFileExist (tmpDir </> "_site" </> "posts" </> "test.html")
              rendered `shouldBe` True
              content <- TIO.readFile (tmpDir </> "_site" </> "posts" </> "test.html")
              T.unpack content `shouldSatisfy` isInfixOf "E2E Industrial Test"
