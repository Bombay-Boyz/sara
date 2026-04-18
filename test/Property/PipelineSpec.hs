{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Property.PipelineSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad (initialState, SaraEnv(..))
import qualified SARA.Routing.Engine as REngine
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified BLAKE3
import qualified Data.ByteString as BS
import UnliftIO.IORef (newIORef)

spec :: Spec
spec = do
  describe "Pipeline Properties" $ do
    it "SlugRoute always results in .html extension" $ do
      let config = defaultConfig
      root <- mkProjectRoot "."
      stateRef <- newIORef initialState
      let _env = SaraEnv config root False [] stateRef Nothing
      
      res <- REngine.resolveRoute SlugRoute "posts/hello.md"
      case res of
        Right (ResolvedRoute outPath) -> do
          outPath `shouldBe` "posts/hello.html"
        _ -> expectationFailure "Expected ResolvedRoute"

    it "Items preserve their metadata through the pipeline" $ do
      let meta = KM.fromList [(K.fromText "test", Aeson.Bool True)]
      let body = "Content"
      let item = Item "test.md" (ResolvedRoute "test.html") meta body (BLAKE3.hash Nothing ([] :: [BS.ByteString]))
      itemMeta item `shouldBe` meta
