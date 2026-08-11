{-# LANGUAGE OverloadedStrings #-}

-- | The bundled default templates are the site's only guaranteed-present
--   HTML; every guarantee this module checks is a guarantee the Web
--   Engineering Standard makes binding for that HTML:
--
--     * exactly one @\<h1\>@, present in the *rendered* output, not just
--       assumed from the source (1.1 of the web standard);
--     * an explicit @lang@ attribute and @\<!DOCTYPE html\>@;
--     * the landmark elements the standard requires in place of bare
--       @\<div\>@s (@\<header\>@, @\<main\>@, @\<footer\>@) (1.2);
--     * a single @\<main\>@;
--     * a @viewport@ meta tag (mobile-first, 0.2);
--     * zero un-audited raw ("triple-mustache") interpolations, which is
--       the exact property 'auditTemplateForRawInterpolation' exists to
--       enforce and the Haskell standard's own architectural guide
--       (SARA_Architecture_and_Implementation_Guide_v1.1.md, section on
--       template security) states as a hard requirement for these three
--       files by name.
--
--   Each template is rendered with a representative context (not just
--   read as text) so a broken Mustache tag or missing landmark inside a
--   conditional section is caught the same way a real build would hit it.
module SARA.TemplateStandardSpec (spec) where

import Test.Hspec
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import Text.Mustache (compileMustacheFile, renderMustache)

import SARA.Security.HtmlEscape (auditTemplateForRawInterpolation)

-- | Every default template, and the context a real build would supply
--   to it (site-level fields plus a representative post/listing).
templatesUnderTest :: [(FilePath, Aeson.Value)]
templatesUnderTest =
  [ ( "templates/post.html"
    , Aeson.object
        [ "siteTitle"  Aeson..= ("Example Site" :: Text)
        , "siteUrl"    Aeson..= ("http://localhost:8080" :: Text)
        , "siteAuthor" Aeson..= ("Example Author" :: Text)
        , "title"      Aeson..= ("Hello, World" :: Text)
        , "author"     Aeson..= ("Example Author" :: Text)
        , "date"       Aeson..= ("2026-01-01" :: Text)
        , "description" Aeson..= ("An example post." :: Text)
        , "itemBody"   Aeson..= ("<p>Body content.</p>" :: Text)
        ]
    )
  , ( "templates/index.html"
    , Aeson.object
        [ "siteTitle"  Aeson..= ("Example Site" :: Text)
        , "siteUrl"    Aeson..= ("http://localhost:8080" :: Text)
        , "siteAuthor" Aeson..= ("Example Author" :: Text)
        , "posts" Aeson..=
            [ Aeson.object
                [ "title"   Aeson..= ("First Post" :: Text)
                , "url"     Aeson..= ("/first-post.html" :: Text)
                , "date"    Aeson..= ("2026-01-01" :: Text)
                , "excerpt" Aeson..= ("A short excerpt." :: Text)
                ]
            ]
        ]
    )
  , ( "templates/default.html"
    , Aeson.object
        [ "siteTitle"  Aeson..= ("Example Site" :: Text)
        , "siteUrl"    Aeson..= ("http://localhost:8080" :: Text)
        , "siteAuthor" Aeson..= ("Example Author" :: Text)
        , "title"      Aeson..= ("A Page" :: Text)
        , "itemBody"   Aeson..= ("<p>Body content.</p>" :: Text)
        ]
    )
  ]

-- | Also verify the empty/degenerate case for the index page: no posts
--   at all must still render a valid, navigable page (Web standard 0.5,
--   "content survives first"), not an empty body or a template error.
emptyIndexContext :: Aeson.Value
emptyIndexContext =
  Aeson.object
    [ "siteTitle"  Aeson..= ("Example Site" :: Text)
    , "siteUrl"    Aeson..= ("http://localhost:8080" :: Text)
    , "siteAuthor" Aeson..= ("Example Author" :: Text)
    , "posts"      Aeson..= ([] :: [Aeson.Value])
    ]

render :: FilePath -> Aeson.Value -> IO Text
render path ctx = do
  tpl <- compileMustacheFile path
  pure . TL.toStrict $ renderMustache tpl ctx

countOccurrences :: Text -> Text -> Int
countOccurrences needle haystack = length (T.breakOnAll needle haystack) 

spec :: Spec
spec = describe "Bundled default templates vs. the Web Engineering Standard" $ do

  describe "Security audit (own project rule: no un-audited {{{ }}})" $
    mapM_
      ( \(path, _) -> it (path <> " has no un-whitelisted raw interpolation") $ do
          src <- TIO.readFile path
          auditTemplateForRawInterpolation path src `shouldBe` []
      )
      templatesUnderTest

  describe "Structural compliance, checked against the rendered HTML" $
    mapM_
      ( \(path, ctx) -> describe path $ do
          it "declares HTML5 doctype" $ do
            html <- render path ctx
            T.strip (T.take 15 (T.stripStart html)) `shouldSatisfy` T.isPrefixOf "<!DOCTYPE html>"

          it "declares an explicit lang attribute" $ do
            html <- render path ctx
            html `shouldSatisfy` T.isInfixOf "<html lang="

          it "includes a mobile-first viewport meta tag" $ do
            html <- render path ctx
            html `shouldSatisfy` T.isInfixOf "name=\"viewport\""

          it "has exactly one <h1>, present in the rendered output" $ do
            html <- render path ctx
            countOccurrences "<h1" html `shouldBe` 1

          it "has exactly one <main> landmark" $ do
            html <- render path ctx
            countOccurrences "<main" html `shouldBe` 1

          it "uses <header> and <footer> landmarks instead of bare divs for them" $ do
            html <- render path ctx
            html `shouldSatisfy` T.isInfixOf "<header"
            html `shouldSatisfy` T.isInfixOf "<footer"
      )
      templatesUnderTest

  describe "templates/index.html degenerate case" $ do
    it "renders a valid, still-navigable page with zero posts" $ do
      html <- render "templates/index.html" emptyIndexContext
      countOccurrences "<h1" html `shouldBe` 1
      html `shouldSatisfy` T.isInfixOf "<nav"
      html `shouldSatisfy` T.isInfixOf "<!DOCTYPE html>"
