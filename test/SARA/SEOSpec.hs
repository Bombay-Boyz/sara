{-# LANGUAGE OverloadedStrings #-}

module SARA.SEOSpec (spec) where

import Test.Hspec
import SARA.SEO.JsonLD
import SARA.Config (defaultConfig)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "JSON-LD Generation" $ do
    it "generates valid Article schema" $ do
      let meta = KM.fromList [(K.fromText "title", Aeson.String "Test Post")]
      let json = generateJsonLD SchemaArticle meta defaultConfig
      case json of
        Aeson.Object obj -> do
          KM.lookup (K.fromText "@type") obj `shouldBe` Just "Article"
          KM.lookup (K.fromText "headline") obj `shouldBe` Just "Test Post"
        _ -> expectationFailure "Expected Aeson Object"
