{-# LANGUAGE OverloadedStrings #-}

-- | Benchmarks for SARA's per-request hot paths (5.6 of the Haskell
--   Engineering Standard: "every module on a path that matters for
--   real-world input size ... has a criterion benchmark checked in").
--
--   Each benchmark below corresponds to a pure function exercised once
--   per source file during a build: frontmatter parsing, path guarding,
--   glob-pattern validation, and HTML escaping. Inputs are scaled
--   (small / medium / large) so a regression that only shows up on
--   realistic input sizes is visible in the CI report, not just on the
--   toy case a developer happened to try first.
module Main (main) where

import Criterion.Main
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K

import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Security.PathGuard (mkProjectRoot, guardPath)
import SARA.Security.GlobGuard (mkGlobPattern)
import SARA.Security.HtmlEscape (escapeHtmlValue)

-- | A YAML frontmatter block with @n@ scalar keys, followed by a body.
--   Mirrors the shape 'SARA.Frontmatter.Parser.parseFrontmatter' is
--   actually handed: a fenced header plus prose.
mkFrontmatterDoc :: Int -> Text
mkFrontmatterDoc n =
  let header = T.unlines [ "k" <> T.pack (show i) <> ": value" <> T.pack (show i) | i <- [1 .. n] ]
      body   = T.replicate (max 1 (n `div` 10)) "Lorem ipsum dolor sit amet. "
  in "---\n" <> header <> "---\n" <> body

-- | A JSON object with @n@ string leaves, used to size 'escapeHtmlValue'
--   input the same way real frontmatter metadata would nest.
mkEscapeTarget :: Int -> Aeson.Value
mkEscapeTarget n =
  Aeson.object
    [ ( K.fromString ("field" <> show i)
      , Aeson.String ("<script>alert(" <> T.pack (show i) <> ")</script>")
      )
    | i <- [1 .. n]
    ]

main :: IO ()
main = do
  root <- mkProjectRoot "."
  defaultMain
    [ -- 'whnf', not 'nf': the 'SaraError'/'GlobPattern'/'SafePath'
      -- results here have no 'NFData' instance (they're small,
      -- already-strict, closed sum/newtype values), so forcing to weak
      -- head normal form is what pins down the branch actually taken
      -- (Left vs. Right) without requiring a deep-force instance that
      -- doesn't exist and shouldn't be added just to satisfy a benchmark.
      bgroup
        "Frontmatter.parseFrontmatter"
        [ bench "10 keys"   $ whnf (parseFrontmatter "bench.md") (mkFrontmatterDoc 10)
        , bench "100 keys"  $ whnf (parseFrontmatter "bench.md") (mkFrontmatterDoc 100)
        , bench "1000 keys" $ whnf (parseFrontmatter "bench.md") (mkFrontmatterDoc 1000)
        ]
    , bgroup
        "PathGuard.guardPath"
        [ bench "within root"       $ whnf (guardPath root) "./posts/a-post-with-a-fairly-long-slug.md"
        , bench "traversal attempt" $ whnf (guardPath root) "./posts/../../etc/passwd"
        ]
    , bgroup
        "GlobGuard.mkGlobPattern"
        [ bench "valid relative glob" $ whnf mkGlobPattern "posts/**/*.md"
        , bench "rejected traversal"  $ whnf mkGlobPattern "posts/../**/*.md"
        ]
    , bgroup
        "HtmlEscape.escapeHtmlValue"
        [ bench "10 fields"   $ nf escapeHtmlValue (mkEscapeTarget 10)
        , bench "100 fields"  $ nf escapeHtmlValue (mkEscapeTarget 100)
        , bench "1000 fields" $ nf escapeHtmlValue (mkEscapeTarget 1000)
        ]
    ]
