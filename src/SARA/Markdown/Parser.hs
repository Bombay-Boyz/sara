{-# LANGUAGE OverloadedStrings #-}

module SARA.Markdown.Parser
  ( parseMarkdown
  ) where

import Data.Text (Text)
import qualified CMarkGFM as CMark
import SARA.Markdown.Shortcode (Shortcode, expandShortcodes)

-- | Parse Markdown to HTML using full GFM extensions.
--   Now supports shortcode expansion before the Markdown pass.
parseMarkdown :: Monad m => (Shortcode -> m Text) -> FilePath -> Text -> m Text
parseMarkdown handler _ body = do
  expandedBody <- expandShortcodes handler body
  let opts = [CMark.optUnsafe, CMark.optSmart]
      exts = [CMark.extTable, CMark.extTaskList, CMark.extAutolink, CMark.extStrikethrough]
  return $ CMark.commonmarkToHtml opts exts expandedBody
