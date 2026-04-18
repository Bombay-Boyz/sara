{-# LANGUAGE OverloadedStrings #-}

module SARA.Markdown.Parser
  ( parseMarkdown
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified CMarkGFM as CMark
import SARA.Markdown.Shortcode (Shortcode, expandShortcodes)

-- | Parse Markdown to HTML using full GFM extensions.
--   Now supports shortcode expansion before the Markdown pass.
parseMarkdown :: Monad m => (Shortcode -> m Text) -> FilePath -> Text -> m Text
parseMarkdown handler _ body = do
  expandedBody <- expandShortcodes handler body
  let opts = [CMark.optUnsafe, CMark.optSmart]
      exts = [CMark.extTable, CMark.extTaskList, CMark.extAutolink, CMark.extStrikethrough]
      html = CMark.commonmarkToHtml opts exts expandedBody
  -- Industrial Grade: Inject MathJax support if math is detected (simple heuristic)
  if "$$" `T.isInfixOf` expandedBody || "$" `T.isInfixOf` expandedBody
    then return $ html <> mathJaxConfig <> mermaidConfig
    else return html

mathJaxConfig :: Text
mathJaxConfig = 
  "<script>window.MathJax = { tex: { inlineMath: [['$', '$'], ['\\\\(', '\\\\)']] } };</script>" <>
  "<script id=\"MathJax-script\" async src=\"https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js\"></script>"

mermaidConfig :: Text
mermaidConfig = 
  "<script type=\"module\">import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs'; mermaid.initialize({ startOnLoad: true });</script>"
