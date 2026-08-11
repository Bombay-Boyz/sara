# Changelog

All notable changes to SARA are documented here. SARA is pre-1.0: until
1.0.0, any release may change the public API (most notably the
`SARA.DSL` surface) without a major version bump, as real-world usage
informs better designs. Pin an exact version if this matters to you.

## 0.1.0.0 — initial public release

First public version. Highlights:

- Core DSL (`match`, `discover`, `route`, `render`, `renderWith`,
  `buildSearchIndex`, `buildSitemap`, `buildRSS`, `loadData`) and
  Shake-based build engine.
- Typed content metadata: `readMarkdownAs` decodes frontmatter into a
  caller-supplied `Aeson.FromJSON` record, with `toRenderableItem` to
  convert back to the default untyped form for the shared
  Mustache/RSS/sitemap/JSON-LD pipeline.
- Security-by-construction: `SARA.Security.PathGuard`, `GlobGuard`,
  `RegexGuard`, `ShellGuard`, and `HtmlEscape`, guarding against path
  traversal, unsafe globs, ReDoS-prone regexes, shell injection, and
  unescaped HTML interpolation.
- `--dry-run` support: reports the full set of planned output files
  without touching disk, by expanding dynamic (`match`/`global`) rule
  declarations the same way a real build would.
- Structured, file-attributed build-issue reporting (SEO audit
  findings and broken internal links), grouped by file at the end of
  a build.
- Migration tooling for Jekyll, Hugo, and Hakyll projects (`sara
  import`).
- Live-reload dev server (`sara serve`).

Tested against GHC 9.4.7 through 9.14.1 (see `tested-with` in
`sara.cabal` and `.github/workflows/ci.yml` for the exact matrix).
