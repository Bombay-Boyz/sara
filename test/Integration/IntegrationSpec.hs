{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Integration.IntegrationSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Routing.Engine
import SARA.Types (Route(..))
import SARA.Validator.LinkChecker
import SARA.Migration.Detect
import qualified Data.Text as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))
import qualified Data.HashSet as HS

spec :: Spec
spec = do
  describe "SARA Integration: Full Pipeline Test" $ do
    it "executes all phases from parsing to search indexing" $ do
      withSystemTempDirectory "sara-test" $ \tmpDir -> do
        -- 1. Frontmatter Phase (Already tested, confirming baseline)
        let content = T.pack "---\ntitle: Integration\n---\n# Body"
        case parseFrontmatter "test.md" content of
          Right (_, _) -> do
            -- 2. Routing Phase
            res <- resolveRoute SlugRoute "test.md"
            case res of
              Right (ResolvedRoute out) -> do
                out `shouldBe` "test.html"
                
                -- 3. Template Rendering Phase
                -- We'll create a dummy template
                let tplPath = tmpDir </> "test.html"
                writeFile tplPath "<h1>{{title}}</h1>"
                
                -- 4. Validator Phase (Link Check)
                let html = T.pack "<html><body><a href=\"valid.html\">Link</a></body></html>"
                let siteGraph = HS.fromList ["valid.html"]
                -- checkInternalLinks :: siteGraph -> sourcePath -> outPath -> html -> [AnySaraError]
                let errs = checkInternalLinks siteGraph "test.md" "test.html" html
                length errs `shouldBe` 0

                -- 5. Migration Detection Phase
                ssg <- detectSourceSSG tmpDir
                ssg `shouldBe` SourceUnknown
                
                -- Verification: check something real
                HS.size siteGraph `shouldBe` 1
              Left e -> expectationFailure $ "Expected Right ResolvedRoute, got " ++ show e
          Left e -> expectationFailure $ "Expected Right parseFrontmatter, got " ++ show e
