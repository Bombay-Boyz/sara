{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Unit.SEOSpec (spec) where

import Test.Hspec
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
  describe "JSON-LD Generation" $ do
    it "generates valid Article schema" $ do
      let meta = KM.fromList [ (K.fromText "title", Aeson.String "Test"), (K.fromText "author", Aeson.String "Tester") ]
      let item = Item "test.md" (ResolvedRoute "test.html") meta "Body" (BLAKE3.hash Nothing ([] :: [BS.ByteString]))
      let json = generateJsonLD Article item
      case json of
        Aeson.Object obj -> do
          KM.lookup (K.fromText "@context") obj `shouldBe` Just "https://schema.org"
          KM.lookup (K.fromText "@type") obj `shouldBe` Just "Article"
          KM.lookup (K.fromText "headline") obj `shouldBe` Just "Test"
          KM.lookup (K.fromText "author") obj `shouldBe` Just "Tester"
        _ -> expectationFailure "Expected Aeson Object"
