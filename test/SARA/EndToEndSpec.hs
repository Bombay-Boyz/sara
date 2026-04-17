module SARA.EndToEndSpec (spec) where

import Test.Hspec
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, setCurrentDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (finally)

spec :: Spec
spec = do
  describe "SARA End-to-End: CLI Execution" $ do
    it "runs 'sara build' and generates site files" $ do
      oldDir <- getCurrentDirectory
      withSystemTempDirectory "sara-e2e" $ \tmpDir -> do
        (`finally` setCurrentDirectory oldDir) $ do
          -- Setup Project
          createDirectory (tmpDir </> "posts")
          createDirectory (tmpDir </> "templates")
          TIO.writeFile (tmpDir </> "posts" </> "test.md") 
            (T.pack "---\ntitle: E2E Industrial Test\ndescription: A test description\nauthor: E2E Tester\n---\n# Hello\nWelcome to SARA.")
          TIO.writeFile (tmpDir </> "templates" </> "post.html") 
            (T.pack "<html><head><title>{{itemTitle}}</title></head><body><h1>{{itemTitle}}</h1></body></html>")
          
          setCurrentDirectory tmpDir
          (exitCode, out, err) <- readProcessWithExitCode 
            "cabal" ["run", "--project-dir", oldDir, "sara", "--", "build"] ""
          
          if exitCode /= ExitSuccess 
            then expectationFailure $ "Build failed: " ++ out ++ "\n" ++ err
            else exitCode `shouldBe` ExitSuccess
