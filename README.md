# SARA

**S**imple, **A**daptive, **R**esponsive **A**rchitecture — a static site generator library for Haskell, built on [Shake](https://shakebuild.com/).

SARA is a library first, in the tradition of [Hakyll](https://jaspervdj.be/hakyll/): you write a small Haskell program (`site.hs`) that describes your site using SARA's combinators, and SARA's Shake-based engine builds it. A thin CLI (`sara build` / `sara serve` / `sara new` / `sara import` / `sara check`) wraps a zero-config default pipeline for getting started without writing any Haskell at all.

## Status

**Pre-1.0.** The public API (the `SARA.DSL` surface in particular) should be expected to change as real usage surfaces better designs — see `CHANGELOG.md`. Pin an exact version if you depend on this.

## What makes SARA different

- **Security-by-construction.** A dedicated `SARA.Security.*` module set guards against path traversal, unsafe globs, ReDoS-prone regexes, and shell injection — checks most static site generators don't attempt at all.
- **Illegal states are unrepresentable.** Content moves through typed pipeline stages (`Item 'Unvalidated` → `Item 'Validated`) enforced by the compiler, not by convention.
- **Content can be typed.** `readMarkdownAs` decodes frontmatter directly into your own record via `Aeson.FromJSON`, catching a malformed or missing field at read time instead of a silent `Nothing` three functions later.
- **The build plan is data.** SARA's DSL doesn't execute side effects directly — it emits a list of declarations (`RuleDecl`) that get interpreted afterward. This is what makes `sara build --dry-run` possible: the exact same plan a real build would use, inspected without touching disk.
- **Migration tooling is built in.** `sara import` detects and helps migrate existing Jekyll, Hugo, or Hakyll projects.

## Quick start

```bash
mkdir myblog && cd myblog
mkdir -p posts templates
cp path/to/sara/templates/*.html templates/
cat > posts/hello.md << 'EOF'
---
title: Hello World
author: Me
---
# Hi there
This is my first post.
EOF
sara build
```

This produces `_site/posts/hello.html`. See `docs/QUICKSTART.md` for more, and `docs/ARCHITECTURE.md` for how the pipeline fits together.

## Building from source

```bash
git clone https://github.com/Bombay-Boyz/sara.git
cd sara
cabal update
cabal build all
cabal test sara-test
```

Requires GHC 9.4.7 through 9.14.1 and `cabal-install`. See `tested-with` in `sara.cabal` for the exact versions covered by CI.

First build resolves and compiles the full dependency set from scratch (Shake, `warp`, `aeson`, `megaparsec`, and friends), which takes a while. Subsequent builds are incremental.

## Documentation

- `docs/QUICKSTART.md` — first site, five minutes
- `docs/ARCHITECTURE.md` — how the DSL, Shake engine, and security layer fit together
- `docs/TUTORIAL_DATA.md`, `docs/TUTORIAL_LUCID.md` — loading external data, using the Lucid-based HTML DSL as an alternative to Mustache templates
- `docs/MIGRATION.md` — moving from Jekyll, Hugo, or Hakyll
- `docs/SECURITY.md` — the threat model the `SARA.Security.*` modules address
- `docs/ERRORS.md` — the structured error taxonomy and how to read error output
- `docs/DEPLOYMENT.md` — deploying a built `_site/`

## License

Mozilla Public License 2.0 — see `LICENSE`.

## Contributing

Before opening a PR: `cabal build all` must succeed with zero warnings (the project builds with `-Werror`), and `cabal test sara-test` must pass. See `haskell-engineering-standard.md` and `web-engineering-standard.md` for the engineering standards this codebase is held to.
