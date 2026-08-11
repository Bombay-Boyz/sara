{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.MigrationSpec (spec) where

import Test.Hspec
import SARA.Migration.Jekyll
import SARA.Migration.Hugo
import SARA.Error (SaraError(..))
import Data.Either (isLeft)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import qualified Data.Text as T

spec :: Spec
spec = do
  describe "Jekyll Migration" $ do
    it "translates highlight tags" $ do
      let input = "{% highlight ruby %}\nputs 'hi'\n{% endhighlight %}"
      let expected = "```ruby\nputs 'hi'\n```"
      translateJekyllShortcodes "test.md" input `shouldBe` Right expected

    it "translates post_url tags" $ do
      let input = "Check this {% post_url 2023-01-01-post %}"
      let expected = "Check this [2023-01-01-post](/posts/2023-01-01-post.html)"
      translateJekyllShortcodes "test.md" input `shouldBe` Right expected

    it "translates link tags" $ do
      let input = "[link]({% link _posts/page.md %})"
      let expected = "[link]([link](_posts/page.md))" 
      -- Note: link translation might need more work but this confirms it acts
      translateJekyllShortcodes "test.md" input `shouldBe` Right expected

    describe "no partial outputs — every unclosed tag fails the whole translation" $ do
      it "fails, naming the tag, on an unclosed {% highlight %} with no {% endhighlight %}" $ do
        let input = "before {% highlight ruby %}\nputs 'hi'\nafter, no closer"
        case translateJekyllShortcodes "broken.md" input of
          Left (MigrationUnclosedTag path _ _) -> path `shouldBe` "broken.md"
          other -> expectationFailure $ "Expected MigrationUnclosedTag, got " ++ show other

      it "fails on a stray {% endhighlight %} with no matching opener" $ do
        let input = "some code\n{% endhighlight %}\nmore text"
        translateJekyllShortcodes "broken.md" input `shouldSatisfy` isLeft

      it "fails, not silently mangles, an unclosed {% post_url ... %}" $ do
        -- Missing the closing " %}" entirely — an earlier version of
        -- this function would have silently swallowed the rest of the
        -- document into the "slug", producing a garbled but
        -- non-erroring link.
        let input = "Check this {% post_url 2023-01-01-post and then just keeps going"
        translateJekyllShortcodes "broken.md" input `shouldSatisfy` isLeft

      it "fails, not silently mangles, an unclosed {% link ... %}" $ do
        let input = "[link]({% link _posts/page.md and no closer here)"
        translateJekyllShortcodes "broken.md" input `shouldSatisfy` isLeft

      it "still succeeds on ordinary content with no tags at all" $ do
        translateJekyllShortcodes "clean.md" "Just plain Markdown, nothing to translate."
          `shouldBe` Right "Just plain Markdown, nothing to translate."

    describe "migrateJekyllPosts (end-to-end: actually reads/writes files, per-file, no partial output)" $ do
      it "migrates a well-formed post and skips a malformed one, writing neither corrupted nor partial output" $
        withSystemTempDirectory "sara-jekyll-migrate" $ \tmpDir -> do
          createDirectoryIfMissing True (tmpDir </> "_posts")
          createDirectoryIfMissing True (tmpDir </> "posts")
          TIO.writeFile (tmpDir </> "_posts" </> "good.md")
            "---\ntitle: Good\n---\nSee {% post_url other-post %} here."
          TIO.writeFile (tmpDir </> "_posts" </> "bad.md")
            "---\ntitle: Bad\n---\nUnclosed {% highlight ruby %}\nno end"

          (migrated, failed) <- migrateJekyllPosts tmpDir tmpDir

          migrated `shouldBe` ["good.md"]
          map fst failed `shouldBe` ["bad.md"]

          -- The good post's translated content actually landed on disk...
          goodContent <- TIO.readFile (tmpDir </> "posts" </> "good.md")
          goodContent `shouldSatisfy` T.isInfixOf "[other-post](/posts/other-post.html)"

          -- ...and the bad post was never written at all — not
          -- skipped-but-copied-verbatim, not partially translated.
          badExists <- doesFileExist (tmpDir </> "posts" </> "bad.md")
          badExists `shouldBe` False

  describe "Hugo Migration" $ do
    it "translates highlight shortcodes (angle-bracket style)" $ do
      let input = "{{< highlight go >}}\nfmt.Println(\"hi\")\n{{< /highlight >}}"
      let expected = "```go\nfmt.Println(\"hi\")\n```"
      translateHugoShortcodes "test.md" input `shouldBe` Right expected

    it "translates highlight shortcodes (percent style)" $ do
      let input = "{{% highlight python %}}\nprint('hi')\n{{% /highlight %}}"
      let expected = "```python\nprint('hi')\n```"
      translateHugoShortcodes "test.md" input `shouldBe` Right expected

    it "translates ref shortcodes" $ do
      let input = "See {{< ref \"other-post.md\" >}} for more."
      let expected = "See [ref](other-post.md) for more."
      translateHugoShortcodes "test.md" input `shouldBe` Right expected

    describe "no partial outputs — every unclosed tag fails the whole translation" $ do
      it "fails on an unclosed {{< highlight >}} with no matching {{< /highlight >}}" $ do
        let input = "before {{< highlight go >}}\nfmt.Println(\"hi\")\nno closer"
        case translateHugoShortcodes "broken.md" input of
          Left (MigrationUnclosedTag path _ _) -> path `shouldBe` "broken.md"
          other -> expectationFailure $ "Expected MigrationUnclosedTag, got " ++ show other

      it "fails on a {{< ref ... >}} missing its opening quote, rather than eating a content character silently" $ do
        -- The previous version unconditionally dropped the length of
        -- an opener that assumed a quote was present, silently eating
        -- the first real character of a malformed, quote-less tag.
        let input = "See {{< ref other-post.md >}} for more."
        translateHugoShortcodes "broken.md" input `shouldSatisfy` isLeft

      it "fails on an unclosed {{< ref \"...\" with no closing quote/>}}" $ do
        let input = "See {{< ref \"other-post.md for more, no closer"
        translateHugoShortcodes "broken.md" input `shouldSatisfy` isLeft

      it "still succeeds on ordinary content with no shortcodes at all" $ do
        translateHugoShortcodes "clean.md" "Just plain Markdown, nothing to translate."
          `shouldBe` Right "Just plain Markdown, nothing to translate."

    describe "migrateHugoContent (end-to-end: recursive discovery, per-file, no partial output)" $ do
      it "migrates a well-formed nested post, flattening its path, and skips a malformed one" $
        withSystemTempDirectory "sara-hugo-migrate" $ \tmpDir -> do
          createDirectoryIfMissing True (tmpDir </> "content" </> "blog")
          TIO.writeFile (tmpDir </> "content" </> "blog" </> "good.md")
            "---\ntitle: Good\n---\nSee {{< ref \"other.md\" >}} here."
          TIO.writeFile (tmpDir </> "content" </> "blog" </> "bad.md")
            "---\ntitle: Bad\n---\nSee {{< ref other.md >}} missing quotes"

          (migrated, failed) <- migrateHugoContent tmpDir tmpDir

          migrated `shouldBe` ["blog/good.md"]
          map fst failed `shouldBe` ["blog/bad.md"]

          goodContent <- TIO.readFile (tmpDir </> "posts" </> "blog-good.md")
          goodContent `shouldSatisfy` T.isInfixOf "[ref](other.md)"

          badFlatExists <- doesFileExist (tmpDir </> "posts" </> "blog-bad.md")
          badFlatExists `shouldBe` False
