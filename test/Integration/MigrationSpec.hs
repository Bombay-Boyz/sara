{-# LANGUAGE OverloadedStrings #-}

module Integration.MigrationSpec (spec) where

import Test.Hspec
import SARA.Migration.Jekyll
import Data.Text (Text)

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
      let expected = "[link](_posts/page.md)" 
      translateJekyllShortcodes "test.md" input `shouldBe` Right expected
