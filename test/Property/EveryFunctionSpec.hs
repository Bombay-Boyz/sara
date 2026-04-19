{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Property.EveryFunctionSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import qualified Data.Text as T
import qualified Data.Aeson as Aeson
import Data.Maybe (isJust, isNothing)

-- Modules to test
import SARA.Security.HtmlEscape (escapeHtmlValue, auditTemplateForRawInterpolation)
import SARA.Types (formatFromText)
import SARA.Migration.Hugo (translateHugoShortcodes)
import SARA.Migration.Jekyll (translateJekyllShortcodes)
import SARA.DSL (chunk)

prop_chunk_size :: Int -> [Int] -> Property
prop_chunk_size n xs =
  n > 0 ==>
  let chunks = chunk n xs
  in all (\c -> length c <= n) chunks

prop_chunk_concat :: Int -> [Int] -> Property
prop_chunk_concat n xs =
  n > 0 ==>
  concat (chunk n xs) === xs

prop_escapeHtmlValue_string :: String -> Property
prop_escapeHtmlValue_string s =
  let t = T.pack s
      val = Aeson.String t
      escaped = escapeHtmlValue val
  in case escaped of
       Aeson.String e -> 
         property $ not ("<" `T.isInfixOf` e) &&
         not (">" `T.isInfixOf` e) &&
         (not ("&" `T.isInfixOf` e) || "&amp;" `T.isInfixOf` e || "&lt;" `T.isInfixOf` e || "&gt;" `T.isInfixOf` e || "&quot;" `T.isInfixOf` e || "&#39;" `T.isInfixOf` e)
       _ -> property False

prop_auditTemplateForRawInterpolation_safe :: String -> Property
prop_auditTemplateForRawInterpolation_safe s =
  -- Exclude raw interpolation tags from generation to ensure it's "safe"
  let t = T.pack s
      safeT = T.replace "{{{" "" $ T.replace "}}}" "" $ T.replace "{{&" "" t
  in null (auditTemplateForRawInterpolation "test.html" safeT) === True

prop_auditTemplateForRawInterpolation_unsafe :: String -> Property
prop_auditTemplateForRawInterpolation_unsafe keyStr =
  let key = T.filter (\c -> c /= '}' && c /= '\n' && c /= '\r') (T.pack keyStr)
  in not (T.null (T.strip key)) ==>
  let t = "{{{" <> key <> "}}}"
      k = T.strip key
      isSafeKey = "SARA_LQIP:" `T.isPrefixOf` k || k == "itemBody"
  in null (auditTemplateForRawInterpolation "test.html" t) === isSafeKey

prop_formatFromText_valid :: Property
prop_formatFromText_valid = 
  forAll (elements ["webp", "avif", "jpeg", "jpg", "png", "WEBP", "JpG"]) $ \t ->
    isJust (formatFromText (T.pack t)) === True

prop_formatFromText_invalid :: String -> Property
prop_formatFromText_invalid s =
  let t = T.pack s
  in T.toLower t `notElem` ["webp", "avif", "jpeg", "jpg", "png"] ==>
  isNothing (formatFromText t) === True

prop_translateHugoShortcodes_highlight :: String -> String -> Property
prop_translateHugoShortcodes_highlight langStr codeStr =
  let lang = T.pack langStr
      code = T.pack codeStr
  in not (" %}" `T.isInfixOf` lang) && not (" %}" `T.isInfixOf` code) && not (" >}}" `T.isInfixOf` lang) ==>
  let input = "{{< highlight " <> lang <> " >}}" <> code <> "{{< /highlight >}}"
      res = translateHugoShortcodes "test.md" input
  in case res of
       Right out -> property $ ("```" <> T.strip lang <> code <> "```") `T.isInfixOf` out
       Left _ -> property False

prop_translateJekyllShortcodes_highlight :: String -> String -> Property
prop_translateJekyllShortcodes_highlight langStr codeStr =
  let lang = T.pack langStr
      code = T.pack codeStr
  in not (" %}" `T.isInfixOf` lang) && not (" %}" `T.isInfixOf` code) ==>
  let input = "{% highlight " <> lang <> " %}" <> code <> "{% endhighlight %}"
      res = translateJekyllShortcodes "test.md" input
  in case res of
       Right out -> property $ ("```" <> T.strip lang <> code <> "```") `T.isInfixOf` out
       Left _ -> property False

spec :: Spec
spec = do
  describe "SARA.Security.HtmlEscape" $ do
    it "escapes unsafe characters in JSON strings" $ property prop_escapeHtmlValue_string
    it "does not flag safe templates" $ property prop_auditTemplateForRawInterpolation_safe
    it "flags unsafe raw interpolation" $ property prop_auditTemplateForRawInterpolation_unsafe

  describe "SARA.Types" $ do
    it "parses valid image formats" $ property prop_formatFromText_valid
    it "rejects invalid image formats" $ property prop_formatFromText_invalid

  describe "SARA.Migration.Hugo" $ do
    it "translates highlight shortcodes" $ property prop_translateHugoShortcodes_highlight

  describe "SARA.Migration.Jekyll" $ do
    it "translates highlight shortcodes" $ property prop_translateJekyllShortcodes_highlight

  describe "SARA.DSL" $ do
    it "chunks lists into proper sizes" $ property prop_chunk_size
    it "preserves all elements when chunking" $ property prop_chunk_concat
