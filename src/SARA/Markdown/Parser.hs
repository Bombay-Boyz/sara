{-# LANGUAGE OverloadedStrings #-}

module SARA.Markdown.Parser
  ( parseMarkdown
  ) where

import Data.Text (Text)
import qualified CMarkGFM as CMark
import SARA.Markdown.Shortcode (Shortcode, expandShortcodes)

-- | Parse Markdown to HTML using full GFM extensions.
--   Supports shortcode expansion before the Markdown pass.
--
--   __Raw HTML__: 'CMark.optUnsafe' is CommonMark's flag to *disable*
--   its built-in HTML sanitizer — with it set, raw @\<script\>@,
--   @\<iframe\>@, and inline event handlers embedded in markdown
--   source pass straight through into rendered output. That flag is
--   now only ever set when the caller explicitly opts in via
--   @allowRawHtml@ (plumbed from 'SARA.Config.cfgAllowRawHtml',
--   default 'False') — CommonMark's default (HTML-escaped literal
--   @\<@\/@\>@\/@&@ in raw HTML) is what every site gets unless its
--   owner has explicitly vouched for its own content. See audit
--   issue #1.
parseMarkdown :: Bool -> (Shortcode -> Text) -> FilePath -> Text -> Text
parseMarkdown allowRawHtml handler _ body = 
  let expandedBody = expandShortcodes handler body
      opts = [CMark.optSmart] <> [CMark.optUnsafe | allowRawHtml]
      exts = [CMark.extTable, CMark.extTaskList, CMark.extAutolink, CMark.extStrikethrough]
  in CMark.commonmarkToHtml opts exts expandedBody
