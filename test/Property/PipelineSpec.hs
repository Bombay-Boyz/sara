{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Property.PipelineSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Routing.Engine
import SARA.SEO.JsonLD
import SARA.Types
import SARA.Config (defaultConfig)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified BLAKE3
import qualified Data.ByteString as BS

spec :: Spec
spec = do
  describe "SARA Integration Pipeline" $ do
    it "processes a complete metadata-to-SEO pipeline" $ do
      let content = "---\ntitle: Integration Test\nauthor: Tester\n---\n# Hello"
      
      -- 1. Parse phase
      case parseFrontmatter "test.md" content of
        Left err -> expectationFailure $ "Parse failed: " ++ show err
        Right (meta, body) -> do
          let item = Item "test.md" (ResolvedRoute "test.html") meta body (BLAKE3.hash Nothing ([] :: [BS.ByteString]))
          
          -- 3. Route phase
          res <- resolveRoute SlugRoute "test.md"
          case res of
            Left err -> expectationFailure $ "Routing failed: " ++ show err
            Right (ResolvedRoute outPath) -> do
              outPath `shouldBe` "test.html"
              
              -- 4. SEO phase
              let json = generateJsonLD Article item
              case json of
                Aeson.Object obj -> do
                  KM.lookup (K.fromText "@context") obj `shouldBe` Just "https://schema.org"
                  KM.lookup (K.fromText "@type") obj `shouldBe` Just "Article"
                _ -> expectationFailure "Expected Aeson Object"
