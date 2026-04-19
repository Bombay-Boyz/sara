{-# LANGUAGE OverloadedStrings #-}
module Integration.EndToEndSpec (spec) where

import Test.Hspec
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectory, setCurrentDirectory, getCurrentDirectory)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Exception (bracket_)

spec :: Spec
spec = do
  describe "SARA End-to-End: CLI Execution" $ do
    it "runs 'sara build' and generates site files" $ do
      oldDir <- getCurrentDirectory
      withSystemTempDirectory "sara-e2e" $ \tmpDir -> do
        bracket_ (setCurrentDirectory tmpDir) (setCurrentDirectory oldDir) $ do
          -- Setup Project
          createDirectory (tmpDir </> "posts")
          createDirectory (tmpDir </> "templates")
          TIO.writeFile (tmpDir </> "posts" </> "test.md") 
            "---\ntitle: E2E Industrial Test\ndescription: A test description\nauthor: E2E Tester\n---\n\n## Hello\nWelcome to SARA."
          TIO.writeFile (tmpDir </> "templates" </> "post.html") 
            "<html><head><title>{{itemMeta.title}}</title><meta name=\"description\" content=\"A very long description that should pass the SARA industrial SEO character limit check easily.\"><meta property=\"og:title\" content=\"{{itemMeta.title}}\"><meta property=\"og:image\" content=\"img.png\"></head><body><h1>{{itemMeta.title}}</h1><main>{{{itemBody}}}</main></body></html>"
          TIO.writeFile (tmpDir </> "sara.yaml")
            "title: E2E Test\nbaseUrl: http://test.com\nauthor: Tester\noutputDir: _site\ndefaultTemplate: templates/post.html\n"
          
          -- Zero-config build (no site.hs)
          (exitCode, out, err) <- readProcessWithExitCode 
            "cabal" ["run", "--project-dir", oldDir, "sara", "--", "build"] ""
          
          if exitCode /= ExitSuccess 
            then expectationFailure $ "Build failed: " ++ out ++ "\n" ++ err
            else exitCode `shouldBe` ExitSuccess
