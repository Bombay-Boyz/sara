{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module SARA.TemplateCheckSpec (spec) where

import Test.Hspec
import SARA.Internal.TemplateCheck
import GHC.Generics (Generic)
import Data.Proxy (Proxy(..))
import Data.Text (Text)

data BlogMeta = BlogMeta
  { title :: Text
  , date  :: Text
  , tags  :: [Text]
  } deriving (Generic)

data EmptyMeta = EmptyMeta deriving (Generic)

spec :: Spec
spec = do
  describe "fieldNamesOf (GHC.Generics field extraction)" $ do
    it "extracts every field name of a record, in declaration order" $
      fieldNamesOf (Proxy @BlogMeta) `shouldBe` ["title", "date", "tags"]

    it "returns an empty list for a record with no fields" $
      fieldNamesOf (Proxy @EmptyMeta) `shouldBe` []

  describe "scanTemplateFields" $ do
    it "extracts a plain variable reference" $
      scanTemplateFields "<title>{{ title }}</title>" `shouldBe` [("title", 1)]

    it "extracts a raw/triple-mustache reference" $
      scanTemplateFields "{{{ itemBody }}}" `shouldBe` [("itemBody", 1)]

    it "extracts a section opener but not its implicit self-reference or closing tag" $
      scanTemplateFields "{{# tags }}{{.}}{{/ tags }}" `shouldBe` [("tags", 1)]

    it "extracts an inverted-section opener" $
      scanTemplateFields "{{^ draft }}Published{{/ draft }}" `shouldBe` [("draft", 1)]

    it "skips comments" $
      scanTemplateFields "{{! just a comment, not a field }}" `shouldBe` []

    it "skips partials" $
      scanTemplateFields "{{> header }}" `shouldBe` []

    it "skips dotted/nested field access (out of scope, not misvalidated)" $
      scanTemplateFields "{{ author.name }}" `shouldBe` []

    it "tracks line numbers correctly across a multi-line template" $
      scanTemplateFields "line one\n{{ title }}\nline three\n{{ date }}"
        `shouldBe` [("title", 2), ("date", 4)]

    it "handles a tag whose own content spans multiple lines" $
      scanTemplateFields "{{#\n  tags\n}}x{{/tags}}" `shouldBe` [("tags", 1)]

    it "finds every reference in a realistic multi-field template" $
      scanTemplateFields "<h1>{{ title }}</h1>\n<p>{{{ itemBody }}}</p>\n{{# tags }}{{.}}{{/tags}}\n{{^ draft }}Live{{/draft}}"
        `shouldBe` [("title", 1), ("itemBody", 2), ("tags", 3), ("draft", 4)]

  describe "unknownTemplateFields" $ do
    it "reports nothing when every reference is a known field" $
      unknownTemplateFields ["title", "date"] "{{ title }} on {{ date }}" `shouldBe` []

    it "reports nothing for always-available context keys not in the typed record" $
      unknownTemplateFields ["title"] "{{ title }} {{ siteTitle }} {{{ itemBody }}}" `shouldBe` []

    it "reports a single unknown field with its line number" $
      unknownTemplateFields ["title"] "{{ tilte }}" `shouldBe` [("tilte", 1)]

    it "reports every occurrence of a repeated unknown field, each with its own line number (more useful for fixing every instance in one pass than silently dropping repeats)" $
      unknownTemplateFields ["title"] "{{ tilte }}\n{{ dat }}\n{{ tilte }}"
        `shouldBe` [("tilte", 1), ("dat", 2), ("tilte", 3)]

    it "end-to-end: a realistic template against a realistic typed record" $
      let tpl = "<title>{{ title }} | {{ siteTitle }}</title><p>{{{ itemBody }}}</p>{{# tags }}{{.}}{{/tags}}"
      in unknownTemplateFields (fieldNamesOf (Proxy @BlogMeta)) tpl `shouldBe` []

    it "end-to-end: catches a real typo against a realistic typed record" $
      let tpl = "<title>{{ tilte }}</title>"
      in unknownTemplateFields (fieldNamesOf (Proxy @BlogMeta)) tpl `shouldBe` [("tilte", 1)]
