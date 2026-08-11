{-# LANGUAGE OverloadedStrings #-}

module SARA.Markdown.Parser
  ( parseMarkdown
  ) where

import Data.Text (Text)
import qualified CMarkGFM as CMark
import SARA.Markdown.Shortcode (Shortcode, expandShortcodes)

-- | Parse Markdown to HTML using full GFM extensions.
--   Now supports shortcode expansion before the Markdown pass.
parseMarkdown :: (Shortcode -> Text) -> FilePath -> Text -> Text
parseMarkdown handler _ body = 
  let expandedBody = expandShortcodes handler body
      opts = [CMark.optUnsafe, CMark.optSmart]
      exts = [CMark.extTable, CMark.extTaskList, CMark.extAutolink, CMark.extStrikethrough]
  in CMark.commonmarkToHtml opts exts expandedBody
