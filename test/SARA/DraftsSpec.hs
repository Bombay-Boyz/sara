{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.DraftsSpec (spec) where

import Test.Hspec
import SARA.Content.Drafts
import SARA.Types
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Time (UTCTime(..), fromGregorian)
import Data.ByteString (ByteString)
import qualified BLAKE3

-- | A minimal, otherwise-valid item carrying only the metadata under
--   test — the fields 'isDraft'/'isFutureDated' don't look at
--   (path, route, body, hash) are filled with harmless placeholders.
mkItem :: Aeson.Object -> ItemP 'Validated Aeson.Object
mkItem meta = Item
  { itemPath  = "posts/test.md"
  , itemRoute = ResolvedRoute "posts/test.html"
  , itemMeta  = meta
  , itemBody  = "body"
  , itemHash  = BLAKE3.hash Nothing ([] :: [ByteString])
  }

referenceNow :: UTCTime
referenceNow = UTCTime (fromGregorian 2026 6 15) 0

spec :: Spec
spec = describe "SARA.Content.Drafts" $ do
  describe "isDraft" $ do
    it "is True for draft: true (YAML boolean)" $
      isDraft (mkItem (KM.fromList [("draft", Aeson.Bool True)])) `shouldBe` True

    it "is True for draft: \"true\" (string form)" $
      isDraft (mkItem (KM.fromList [("draft", Aeson.String "true")])) `shouldBe` True

    it "is True for draft: \"yes\", case-insensitively" $
      isDraft (mkItem (KM.fromList [("draft", Aeson.String "YES")])) `shouldBe` True

    it "is False for draft: false" $
      isDraft (mkItem (KM.fromList [("draft", Aeson.Bool False)])) `shouldBe` False

    it "is False when the draft field is absent (publish by default)" $
      isDraft (mkItem KM.empty) `shouldBe` False

  describe "isFutureDated" $ do
    it "is True for a bare date strictly after now" $
      isFutureDated referenceNow (mkItem (KM.fromList [("date", Aeson.String "2026-12-25")]))
        `shouldBe` True

    it "is False for a bare date on or before now" $
      isFutureDated referenceNow (mkItem (KM.fromList [("date", Aeson.String "2026-01-01")]))
        `shouldBe` False

    it "is False when the date field is absent" $
      isFutureDated referenceNow (mkItem KM.empty) `shouldBe` False

    it "is False (publishes it), not an error, for an unparseable date — see Haddock on isFutureDated" $
      isFutureDated referenceNow (mkItem (KM.fromList [("date", Aeson.String "not-a-real-date")]))
        `shouldBe` False

  describe "isPublishable" $ do
    it "is False for a draft even with a past date" $
      isPublishable referenceNow (mkItem (KM.fromList
        [ ("draft", Aeson.Bool True)
        , ("date", Aeson.String "2020-01-01")
        ])) `shouldBe` False

    it "is False for a future-dated, non-draft post" $
      isPublishable referenceNow (mkItem (KM.fromList [("date", Aeson.String "2030-01-01")]))
        `shouldBe` False

    it "is True for an ordinary, non-draft, past-dated post" $
      isPublishable referenceNow (mkItem (KM.fromList [("date", Aeson.String "2020-01-01")]))
        `shouldBe` True

    it "is True for a post with no draft/date fields at all" $
      isPublishable referenceNow (mkItem KM.empty) `shouldBe` True
