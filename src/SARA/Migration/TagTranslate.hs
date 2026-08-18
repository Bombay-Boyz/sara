{-# LANGUAGE OverloadedStrings #-}

-- | Shared "find a balanced opener\/closer span and translate what's
--   inside it" helpers, used by both 'SARA.Migration.Jekyll' and
--   'SARA.Migration.Hugo'. Both migration sources have tag-like
--   syntax shaped this way (Jekyll's Liquid tags, Hugo's shortcodes),
--   and both need the same "translate every occurrence, or fail
--   naming the first one with no closer — never a partial\/garbled
--   result" contract.
module SARA.Migration.TagTranslate
  ( translateBalanced
  , translateHighlightLike
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Find the next @opener ... closer@ span, apply 'render' to the text
--   between them, and recurse on what follows — or fail with
--   'MigrationUnclosedTag' if 'opener' appears with no 'closer' before
--   the end of the text.
translateBalanced
  :: FilePath
  -> Text                -- ^ opener, e.g. "{% highlight "
  -> Text                -- ^ closer, e.g. " %}"
  -> (Text -> Text)      -- ^ render the tag's inner content as its replacement
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateBalanced path opener closer render = go
  where
    go t =
      let (before, match) = T.breakOn opener t
      in if T.null match
         then Right t  -- no (more) occurrences of this tag: done, unchanged
         else
           let rest = T.drop (T.length opener) match
               (inner, after) = T.breakOn closer rest
           in if T.null after
              then Left $ MigrationUnclosedTag path opener closer
              else do
                restResult <- go (T.drop (T.length closer) after)
                pure (before <> render inner <> restResult)

-- | Find the next @opener lang langCloser code tagCloser@ span — the
--   shape a single-style fenced "highlight" tag has in both Jekyll
--   (@{% highlight lang %} code {% endhighlight %}@) and Hugo's own
--   single-style case (@{{\< highlight lang \>}} code {{\< \/highlight \>}}@)
--   — render it as a Markdown fenced code block, and recurse on what
--   follows. Also treats a stray 'tagCloser' with no preceding
--   'opener' as malformed, the same as both call sites did before
--   this was factored out: a lone @{% endhighlight %}@ (or its Hugo
--   equivalent) with nothing to close is itself a sign of a
--   corrupted or hand-edited source file, not something to pass
--   through silently.
--
--   Not used for Hugo's dual angle\/percent-style dispatch — that
--   picks whichever style's opener occurs *first* at each step, which
--   isn't the same algorithm as scanning for one style at a time (two
--   sequential single-style passes could misfire if one style's
--   rendered code body happens to contain the other style's literal
--   tag text) — so 'SARA.Migration.Hugo.translateHugoHighlight' keeps
--   its own dispatch logic rather than reusing this.
translateHighlightLike
  :: FilePath
  -> Text            -- ^ opener, e.g. "{% highlight "
  -> Text            -- ^ closes the language tag, e.g. " %}"
  -> Text            -- ^ closes the code body, e.g. "{% endhighlight %}"
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateHighlightLike path opener langCloser tagCloser = go
  where
    go t =
      let (before, match) = T.breakOn opener t
      in if T.null match
         then
           if tagCloser `T.isInfixOf` t
           then Left $ MigrationUnclosedTag path tagCloser opener
           else Right t
         else
           let rest = T.drop (T.length opener) match
               (lang, afterOpen) = T.breakOn langCloser rest
           in if T.null afterOpen
              then Left $ MigrationUnclosedTag path opener langCloser
              else
                let body = T.drop (T.length langCloser) afterOpen
                    (code, afterClose) = T.breakOn tagCloser body
                in if T.null afterClose
                   then Left $ MigrationUnclosedTag path (opener <> "...") tagCloser
                   else do
                     restResult <- go (T.drop (T.length tagCloser) afterClose)
                     pure (before <> "```" <> T.strip lang <> code <> "```" <> restResult)
