# SARA Production Readiness Checklist (WIP)

This document tracks the elimination of all stubs, placeholders, and technical debt. No code is considered production-ready until this list is cleared.

## Phase 1: Core Logic & Build Correctness
- [ ] **Routing Engine**: Implement capture group interpolation (`\1`, `\2`, etc.) for `RegexRoute` in `src/SARA/Routing/Engine.hs`.
- [ ] **DSL Metadata**: Implement `remapMetadata` logic to allow site-wide key transformations in `src/SARA/DSL.hs`.
- [ ] **Frontmatter Parser**: Implement full `TOML` to `Aeson.Object` conversion in `src/SARA/Frontmatter/Parser.hs`.
- [ ] **Build Planner**: Implement `RuleGlobal` support and expand template context to include site-wide metadata in `src/SARA/Internal/Planner.hs`.

## Phase 2: System Robustness
- [ ] **Live Reload Server**: Implement connection heartbeats and filtering of closed sockets to prevent resource leaks in `src/SARA/LiveReload/Server.hs`.
- [ ] **Asset Pipeline**: Add binary presence verification for `avifenc` and `cwebp` with graceful fallbacks in `src/SARA/Asset/Image.hs`.

## Phase 3: Industrial Features
- [ ] **Search Index**: Implement Lunr.js compatible JSON index generation in `src/SARA/Search/Index.hs`.
- [ ] **Migration (Hugo)**: Implement Hugo-to-SARA shortcode and metadata translator in `src/SARA/Migration/Hugo.hs`.
- [ ] **Migration (Hakyll)**: Implement Hakyll-to-SARA project structure translator in `src/SARA/Migration/Hakyll.hs`.

## Phase 4: Documentation
- [ ] **Architecture**: Populate `docs/ARCHITECTURE.md`.
- [ ] **Error Reference**: Populate `docs/ERRORS.md`.
- [ ] **Migration Guide**: Populate `docs/MIGRATION.md`.
- [ ] **Quickstart**: Populate `docs/QUICKSTART.md`.
