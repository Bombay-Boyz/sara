{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module SARA.Migration.Jekyll
  ( translateJekyllShortcodes
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Translates common Jekyll Liquid tags to Markdown/SARA equivalents.
translateJekyllShortcodes
  :: FilePath
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateJekyllShortcodes _ content =
  Right $ translateHighlight 
        $ translateLink 
        $ translatePostUrl content

-- {% post_url 2010-06-15-my-post %} -> [my-post](/posts/2010-06-15-my-post.html)
-- Simplified translation for now.
translatePostUrl :: Text -> Text
translatePostUrl t = 
  let (before, matchFound) = T.breakOn "{% post_url " t
  in if T.null matchFound
     then t
     else 
       let rest = T.drop (T.length "{% post_url ") matchFound
           (slug, after) = T.breakOn " %}" rest
           url = "/posts/" <> T.strip slug <> ".html"
           link = "[" <> T.strip slug <> "](" <> url <> ")"
       in before <> link <> translatePostUrl (T.drop 3 after)

-- {% link _posts/2010-06-15-my-post.md %} -> /posts/2010-06-15-my-post.html
translateLink :: Text -> Text
translateLink t = 
  let (before, matchFound) = T.breakOn "{% link " t
  in if T.null matchFound
     then t
     else 
       let rest = T.drop (T.length "{% link ") matchFound
           (path, after) = T.breakOn " %}" rest
           -- Industrial fix: Resolve Jekyll-style internal links to SARA paths
           cleanPath = T.strip path
           url = if ".md" `T.isSuffixOf` cleanPath
                 then T.replace "_posts/" "/posts/" (T.replace ".md" ".html" cleanPath)
                 else cleanPath
       in before <> url <> translateLink (T.drop 3 after)

-- {% highlight ruby %} -> ```ruby
-- {% endhighlight %} -> ```
translateHighlight :: Text -> Text
translateHighlight t = 
  T.replace "{% endhighlight %}" "```" $
  let (before, match) = T.breakOn "{% highlight " t
  in if T.null match
     then t
     else 
       let rest = T.drop (T.length "{% highlight ") match
           (lang, after) = T.breakOn " %}" rest
           fence = "```" <> T.strip lang
       in before <> fence <> translateHighlight (T.drop 3 after)
