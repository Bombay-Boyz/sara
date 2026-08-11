{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module SARA.PipelineSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Routing.Engine
import SARA.Types (Route(..))
import SARA.SEO.JsonLD
import SARA.Config (defaultConfig)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "SARA Integration Pipeline" $ do
    it "processes a complete metadata-to-SEO pipeline" $ do
      -- 1. Simulated input from a file
      let content = T.pack "---\ntitle: Integration Test\n---\n# Hello World"
      
      -- 2. Parsing phase
      let parsed = parseFrontmatter "test.md" content
      case parsed of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right (meta, _) -> do
          -- 3. Routing phase
          let routed = resolveRoute SlugRoute "test.md"
          case routed of
            Left err -> expectationFailure $ "Routing failed: " ++ show err
            Right (ResolvedRoute outPath) -> do
              outPath `shouldBe` "test.html"
              
              -- 4. SEO phase
              let json = generateJsonLD SchemaArticle meta defaultConfig
              case json of
                Aeson.Object obj -> do
                  KM.lookup (K.fromText "@context") obj `shouldBe` Just "https://schema.org"
                  KM.lookup (K.fromText "@type") obj `shouldBe` Just "Article"
                _ -> expectationFailure "Expected Aeson Object"
