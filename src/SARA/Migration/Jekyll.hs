{-# LANGUAGE OverloadedStrings #-}

module SARA.Migration.Jekyll
  ( translateJekyllShortcodes
  , migrateJekyllPosts
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import SARA.Migration.TagTranslate (translateBalanced, translateHighlightLike)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Control.Monad (forM)

-- | Translates common Jekyll Liquid tags to Markdown/SARA equivalents.
--   Total: either every recognised tag translates successfully, or the
--   whole translation fails with 'MigrationUnclosedTag' naming exactly
--   which tag has no closer — never a partially-translated result. An
--   earlier version of this function always returned 'Right' and, on
--   an unclosed @{% post_url ...@ / @{% link ...@ / @{% highlight ...@
--   tag, silently swallowed the rest of the file into a garbled
--   "replacement" (since 'Data.Text.breakOn' returning "not found" and
--   "found the rest of the document as the match" were being treated
--   as the same case). That's a worse outcome for a migration tool
--   than failing outright: a user reviewing migrated output has no way
--   to distinguish "this Markdown is correct" from "this Markdown is
--   corrupted by a bug," so it must never happen silently.
translateJekyllShortcodes
  :: FilePath
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateJekyllShortcodes path content =
  translatePostUrl path content
    >>= translateLink path
    >>= translateHighlight path

-- {% post_url 2010-06-15-my-post %} -> [my-post](/posts/2010-06-15-my-post.html)
translatePostUrl :: FilePath -> Text -> Either (SaraError 'EKMigration) Text
translatePostUrl path = translateBalanced path "{% post_url " " %}" $ \slug ->
  let stripped = T.strip slug
  in "[" <> stripped <> "](/posts/" <> stripped <> ".html)"

-- {% link _posts/2010-06-15-my-post.md %} -> [link](/posts/2010-06-15-my-post.html)
translateLink :: FilePath -> Text -> Either (SaraError 'EKMigration) Text
translateLink path = translateBalanced path "{% link " " %}" $ \p ->
  "[link](" <> T.strip p <> ")"

-- {% highlight ruby %} -> ```ruby ... {% endhighlight %} -> ```
--
-- Handled as one combined opener/closer pair (from "{% highlight " to
-- the matching "{% endhighlight %}"), not as two independently-replaced
-- tags: the previous version replaced every "{% endhighlight %}"
-- globally before even looking for "{% highlight ", so mismatched
-- counts (an extra endhighlight, or a highlight with no matching end)
-- were never detected — exactly the silent-partial-output class this
-- rewrite closes. Uses 'translateHighlightLike' (shared with
-- 'SARA.Migration.Hugo''s single-style case), rather than its own
-- copy of the same three-delimiter scan.
translateHighlight :: FilePath -> Text -> Either (SaraError 'EKMigration) Text
translateHighlight path =
  translateHighlightLike path "{% highlight " " %}" "{% endhighlight %}"

-- | Migrates every post under @sourceRoot\/_posts@ into @destRoot\/posts@.
--
--   Per-file, not whole-migration, atomicity: one malformed post must
--   not block migrating the other N-1 — a real Jekyll blog with years
--   of posts shouldn't have its entire migration blocked by one file
--   with an unclosed tag. But per *file*, the same "no partial
--   outputs" rule from 'translateJekyllShortcodes' still applies
--   absolutely: a file is either translated completely and written in
--   full, or not written at all and reported as failed — there is no
--   third outcome where a mangled version gets written silently.
--
--   Returns the filenames that migrated successfully and the
--   filenames that didn't, each paired with exactly why — the caller
--   (currently 'app/Main.hs'\'s @runImport@) is responsible for
--   presenting that to the person running the migration; this
--   function's contract is just "never silently drop or corrupt a
--   file," not "decide how to report it."
migrateJekyllPosts :: FilePath -> FilePath -> IO ([FilePath], [(FilePath, SaraError 'EKMigration)])
migrateJekyllPosts sourceRoot destRoot = do
  let postsDir = sourceRoot </> "_posts"
  exists <- doesDirectoryExist postsDir
  if not exists
    then pure ([], [])
    else do
      files <- listDirectory postsDir
      let postFiles = filter (\f -> takeExtension f `elem` [".md", ".markdown"]) files
      results <- forM postFiles $ \f -> do
        let srcPath = postsDir </> f
        content <- TIO.readFile srcPath
        case translateJekyllShortcodes srcPath content of
          Right translated -> do
            TIO.writeFile (destRoot </> "posts" </> f) translated
            pure (Right f)
          Left err -> pure (Left (f, err))
      pure ([f | Right f <- results], [fe | Left fe <- results])
