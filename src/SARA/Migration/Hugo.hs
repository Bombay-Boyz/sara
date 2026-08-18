{-# LANGUAGE OverloadedStrings #-}

module SARA.Migration.Hugo
  ( translateHugoShortcodes
  , migrateHugoContent
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import SARA.FileSystem (listFilesRecursiveFrom)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesDirectoryExist, createDirectoryIfMissing)
import System.FilePath (takeExtension, (</>))
import Control.Monad (forM)

-- | Translates common Hugo shortcodes to Markdown/SARA equivalents.
--   Total, for the same reason as 'SARA.Migration.Jekyll.translateJekyllShortcodes':
--   an unclosed @{{\< highlight ...@ \/ @{{\< ref ...@ tag fails this
--   translation outright, naming the unclosed tag, rather than
--   silently producing a partially- or wrongly-translated file.
translateHugoShortcodes
  :: FilePath
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateHugoShortcodes path content =
  translateHugoRef path content
    >>= translateHugoHighlight path

-- {{< highlight go >}} -> ```go ... {{< /highlight >}} -> ```
-- Also accepts the "{{% ... %}}" variant Hugo allows for the same
-- shortcode. Paired (opener matched to its own closer, of either
-- style) rather than the previous version's two independent global
-- replacements of "{{< /highlight >}}" and "{{% /highlight %}}",
-- which — like Jekyll's equivalent bug — never detected a mismatched
-- open/close count at all.
translateHugoHighlight :: FilePath -> Text -> Either (SaraError 'EKMigration) Text
translateHugoHighlight path = go
  where
    angleOpen  = "{{< highlight "
    angleClose = "{{< /highlight >}}"
    pctOpen    = "{{% highlight "
    pctClose   = "{{% /highlight %}}"

    go t =
      let (beforeAngle, matchAngle) = T.breakOn angleOpen t
          (beforePct, matchPct)     = T.breakOn pctOpen t
      in case (T.null matchAngle, T.null matchPct) of
           (True, True) ->
             -- No opener of either style left. A stray, unmatched
             -- closer with no preceding opener is still malformed.
             if angleClose `T.isInfixOf` t || pctClose `T.isInfixOf` t
             then Left $ MigrationUnclosedTag path "{{< /highlight >}} or {{% /highlight %}}" "{{< highlight ... or {{% highlight ..."
             else Right t
           (False, True) -> handleOne angleOpen " >}}" angleClose beforeAngle matchAngle
           (True, False) -> handleOne pctOpen " %}}" pctClose beforePct matchPct
           (False, False) ->
             -- Both styles present later in the text: handle whichever
             -- occurs first, so nesting order is preserved.
             if T.length beforeAngle <= T.length beforePct
             then handleOne angleOpen " >}}" angleClose beforeAngle matchAngle
             else handleOne pctOpen " %}}" pctClose beforePct matchPct

    handleOne opener openCloseDelim tagCloser before match = do
      let rest = T.drop (T.length opener) match
          (lang, afterOpen) = T.breakOn openCloseDelim rest
      if T.null afterOpen
        then Left $ MigrationUnclosedTag path opener openCloseDelim
        else do
          let body = T.drop (T.length openCloseDelim) afterOpen
              (code, afterClose) = T.breakOn tagCloser body
          if T.null afterClose
            then Left $ MigrationUnclosedTag path (opener <> "...") tagCloser
            else do
              restResult <- go (T.drop (T.length tagCloser) afterClose)
              pure (before <> "```" <> T.strip lang <> code <> "```" <> restResult)

-- {{< ref "path" >}} -> [ref](path)
--
-- The previous version matched on "{{< ref " (no quote) and then
-- unconditionally dropped the length of "{{< ref \"" (quote included)
-- from that match — silently assuming the very next character was a
-- quote without checking. A tag missing its opening quote would
-- therefore have its first content character eaten silently. Verified
-- explicitly here instead.
translateHugoRef :: FilePath -> Text -> Either (SaraError 'EKMigration) Text
translateHugoRef path = go
  where
    openKeyword = "{{< ref "
    closer = "\" >}}"

    go t =
      let (before, match) = T.breakOn openKeyword t
      in if T.null match
         then Right t
         else
           let afterKeyword = T.drop (T.length openKeyword) match
           in case T.uncons afterKeyword of
                Just ('"', rest) ->
                  let (refPath, after) = T.breakOn closer rest
                  in if T.null after
                     then Left $ MigrationUnclosedTag path (openKeyword <> "\"") closer
                     else do
                       restResult <- go (T.drop (T.length closer) after)
                       pure (before <> "[ref](" <> T.strip refPath <> ")" <> restResult)
                _ -> Left $ MigrationUnclosedTag path openKeyword "\" (missing opening quote right after \"ref \")"

-- | Migrates every Markdown file found (recursively) under
--   @sourceRoot\/content@ into a flat @destRoot\/posts@ — Hugo content
--   is organised into arbitrary nested sections, but SARA's default
--   scaffold expects a flat @posts\/*.md@ glob, so files are renamed
--   with their relative path segments joined by @-@ to avoid
--   collisions between e.g. @content\/blog\/intro.md@ and
--   @content\/docs\/intro.md@ rather than silently overwriting one
--   with the other.
--
--   Same per-file guarantee as 'SARA.Migration.Jekyll.migrateJekyllPosts':
--   one malformed file is reported and skipped, not allowed to block
--   the rest of the migration or to be written in a partially-translated
--   state.
migrateHugoContent :: FilePath -> FilePath -> IO ([FilePath], [(FilePath, SaraError 'EKMigration)])
migrateHugoContent sourceRoot destRoot = do
  let contentDir = sourceRoot </> "content"
  exists <- doesDirectoryExist contentDir
  if not exists
    then pure ([], [])
    else do
      relPaths <- findMarkdownFilesRecursive contentDir ""
      createDirectoryIfMissing True (destRoot </> "posts")
      results <- forM relPaths $ \relPath -> do
        let srcPath = contentDir </> relPath
        content <- TIO.readFile srcPath
        case translateHugoShortcodes srcPath content of
          Right translated -> do
            let flatName = T.unpack (T.replace "/" "-" (T.pack relPath))
            TIO.writeFile (destRoot </> "posts" </> flatName) translated
            pure (Right relPath)
          Left err -> pure (Left (relPath, err))
      pure ([f | Right f <- results], [fe | Left fe <- results])

-- | All @.md@ file paths under 'dir', relative to 'dir'. Filters
--   'SARA.FileSystem.listFilesRecursiveFrom' (the same recursive-walk
--   helper 'app/Main.hs' uses) rather than re-deriving the walk here.
findMarkdownFilesRecursive :: FilePath -> FilePath -> IO [FilePath]
findMarkdownFilesRecursive baseDir relSoFar =
  filter ((== ".md") . takeExtension) <$> listFilesRecursiveFrom baseDir relSoFar
