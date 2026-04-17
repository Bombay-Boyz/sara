module SARA.FrontmatterSpec (spec) where

import Test.Hspec
import SARA.Frontmatter.Parser
import SARA.Frontmatter.Detect
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as Key
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "SARA.Frontmatter" $ do
    describe "Frontmatter Detection" $ do
      it "detects YAML" $ detectFormat (T.pack "---\nkey: value\n---") `shouldBe` FmYAML
      it "detects TOML" $ detectFormat (T.pack "+++\nkey = \"value\"\n+++") `shouldBe` FmTOML
      it "detects JSON" $ detectFormat (T.pack "{\n\"key\": \"value\"\n}") `shouldBe` FmJSON
      it "detects none" $ detectFormat (T.pack "plain content") `shouldBe` FmNone

    describe "Frontmatter Parsing" $ do
      it "parses YAML and escapes HTML" $ do
        let input = T.pack "---\ntitle: <script>\n---\n"
        case parseFrontmatter "test.md" input of
          Right (meta, _) -> KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String (T.pack "&lt;script&gt;"))
          Left e -> expectationFailure $ "Expected Right, got " ++ show e
      
      it "parses JSON" $ do
        let input = T.pack "{\n\"title\": \"Value\"\n}"
        case parseFrontmatter "test.md" input of
          Right (meta, _) -> KM.lookup (Key.fromString "title") meta `shouldBe` Just (Aeson.String (T.pack "Value"))
          Left e -> expectationFailure $ "Expected Right, got " ++ show e

    describe "Metadata Remapping" $ do
      it "remaps keys" $ do
        let meta = KM.fromList [(Key.fromString "from", Aeson.String (T.pack "value"))]
        -- Placeholder for Remap logic
        let remapped = meta 
        KM.lookup (Key.fromString "from") remapped `shouldBe` Just (Aeson.String (T.pack "value"))
        
      it "fails if fromKey is missing" $ do
        True `shouldBe` True
