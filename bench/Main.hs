{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion.Main
import SARA.Markdown.Parser
import SARA.Frontmatter.Parser
import SARA.Frontmatter.Detect
import qualified Data.Text as T

main :: IO ()
main = defaultMain
  [ bgroup "Markdown"
    [ bench "small (1KB)" $ whnf (parseMarkdown (\_ -> "") "test.md") smallMarkdown
    , bench "large (100KB)" $ whnf (parseMarkdown (\_ -> "") "test.md") largeMarkdown
    ]
  , bgroup "Frontmatter"
    [ bench "detection (YAML)" $ whnf detectFormat yamlContent
    , bench "parsing (YAML)" $ whnf (parseFrontmatter "test.md") yamlContent
    ]
  ]

smallMarkdown :: T.Text
smallMarkdown = "# Hello\nThis is a test of SARA performance."

largeMarkdown :: T.Text
largeMarkdown = T.replicate 1000 smallMarkdown

yamlContent :: T.Text
yamlContent = "---\ntitle: Benchmark\nauthor: SARA\ndescription: Performance testing\n---\nBody content here."
