-- | A single, shared recursive directory walk, used by two call sites
--   that previously hand-rolled the identical traversal independently:
--   'SARA.Migration.Hugo.findMarkdownFilesRecursive' (filtered to
--   @.md@) and @app/Main.hs@'s @listFilesRecursive@ (unfiltered, full
--   paths). Neither filtering nor path-prefixing is this module's job —
--   callers apply those on top of the plain, relative file list below.
module SARA.FileSystem
  ( listFilesRecursiveFrom
  ) where

import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM)

-- | All file paths found by a plain recursive walk of @baseDir \<\/\>
--   relSoFar@, returned relative to 'baseDir' (so a caller that wants
--   the walk to start at 'baseDir' itself passes @""@ for
--   'relSoFar' — that's also how each recursive step re-enters this
--   function for a subdirectory it finds along the way).
listFilesRecursiveFrom :: FilePath -> FilePath -> IO [FilePath]
listFilesRecursiveFrom baseDir relSoFar = do
  entries <- listDirectory (baseDir </> relSoFar)
  fmap concat . forM entries $ \name -> do
    let rel = if null relSoFar then name else relSoFar </> name
    isDir <- doesDirectoryExist (baseDir </> rel)
    if isDir
      then listFilesRecursiveFrom baseDir rel
      else pure [rel]
