{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module SARA.FrontmatterSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Frontmatter.Detect
import SARA.Frontmatter.Remap (remapMetadata)
import SARA.Error (SaraError(..))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T
import Data.Either (isLeft)

spec :: Spec
spec = do
  describe "SARA.Frontmatter" $ do
    describe "Frontmatter Detection" $ do
      it "detects YAML" $ detectFormat (T.pack "---\nkey: value\n---") `shouldBe` FmYAML
      it "detects TOML" $ detectFormat (T.pack "+++\nkey = \"value\"\n+++") `shouldBe` FmTOML
      it "detects JSON" $ detectFormat (T.pack "{\n\"key\": \"value\"\n}") `shouldBe` FmJSON
      it "detects none" $ detectFormat (T.pack "plain content") `shouldBe` FmNone

      it "still detects YAML frontmatter after a leading UTF-8 BOM (Jekyll's own docs warn this silently breaks parsing otherwise)" $
        parseFrontmatter "bom.md" (T.pack "\xFEFF---\ntitle: Has A BOM\n---\nBody.") `shouldSatisfy`
          (\case
            Right (meta, body) ->
              KM.lookup (Key.fromString "title") meta == Just (Aeson.String (T.pack "Has A BOM"))
                && T.strip body == T.pack "Body."
            Left _ -> False)

    describe "Frontmatter Parsing" $ do
      it "parses YAML and returns metadata values exactly as authored (escaping is the renderer's job, not the parser's — see Haddock on parseFrontmatter)" $ do
        let input = T.pack "---\ntitle: <script>\n---\n"
        case parseFrontmatter "test.md" input of
          Right (meta, _) -> KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String (T.pack "<script>"))
          Left e -> expectationFailure $ "Expected Right, got " ++ show e
      
      it "parses JSON" $ do
        let input = T.pack "{\n\"title\": \"Value\"\n}"
        case parseFrontmatter "test.md" input of
          Right (meta, _) -> KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String (T.pack "Value"))
          Left e -> expectationFailure $ "Expected Right, got " ++ show e

    describe "Metadata Remapping (SARA.Frontmatter.Remap.remapMetadata)" $ do
      it "renames 'from' to 'to', removing the old key" $ do
        let meta = KM.fromList [(Key.fromString "from", Aeson.String (T.pack "value"))]
        case remapMetadata [(T.pack "from", T.pack "to")] "test.md" meta of
          Right remapped -> do
            KM.lookup (Key.fromString "to") remapped `shouldBe` Just (Aeson.String (T.pack "value"))
            KM.lookup (Key.fromString "from") remapped `shouldBe` Nothing
          Left e -> expectationFailure $ "Expected Right, got " ++ show e

      it "preserves the existing 'to' value and drops 'from' if 'to' already exists" $ do
        let meta = KM.fromList
              [ (Key.fromString "from", Aeson.String (T.pack "old"))
              , (Key.fromString "to",   Aeson.String (T.pack "kept"))
              ]
        case remapMetadata [(T.pack "from", T.pack "to")] "test.md" meta of
          Right remapped -> do
            KM.lookup (Key.fromString "to") remapped `shouldBe` Just (Aeson.String (T.pack "kept"))
            KM.lookup (Key.fromString "from") remapped `shouldBe` Nothing
          Left e -> expectationFailure $ "Expected Right, got " ++ show e

      it "fails with FrontmatterRemapMissing if fromKey is missing" $ do
        let meta = KM.fromList [(Key.fromString "unrelated", Aeson.String (T.pack "value"))]
        let result = remapMetadata [(T.pack "from", T.pack "to")] "test.md" meta
        isLeft result `shouldBe` True
        case result of
          Left (FrontmatterRemapMissing path key) -> do
            path `shouldBe` "test.md"
            key `shouldBe` T.pack "from"
          _ -> expectationFailure "Expected FrontmatterRemapMissing"
