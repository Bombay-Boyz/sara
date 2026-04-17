# SARA Architectural & Implementation Improvements

This document tracks identified weaknesses in the SARA static site engine that must be addressed to transition from "Production-Ready" to "World-Class" status.

## 1. Clean Idiomatic Code
- **Mutable State in DSL**: Replace `IORef` usage in `SaraEnv` (`envSiteGraph`, `envRemapRules`) with a pure state accumulation pattern or a `StateT` layer to improve testability and declarative purity.
- **Monolithic Planner**: Decompose the `translateDecl` function in `src/SARA/Internal/Planner.hs` into specialized sub-generators to improve maintainability.

## 2. ADTs, GADTs, and Algorithms
- **Brittle Regex Interpolation**: Refactor `interpolateCaptures` in `src/SARA/Routing/Engine.hs` to use a formal parser (e.g., `megaparsec`) instead of manual recursion.
- **Search Scalability**: Transition the search index from a flat JSON array to a proper **inverted index** or trie-based structure to support large-scale sites.

## 3. Concurrency and Parallelism
- **Watcher Debouncing**: Implement a debouncing mechanism in the file watcher (`app/Main.hs`) to prevent redundant builds during multi-file filesystem events (e.g., `git checkout`).
- **Parallel Broadcasting**: Refactor `src/SARA/LiveReload/Server.hs` to use `Control.Concurrent.Async` for parallel WebSocket broadcasting.

## 4. Unsafe Functions and Partials
- **Command Type Safety**: Refactor the `Command` ADT in `app/Main.hs` to eliminate partial field access (e.g., `bldDryRun`), preventing potential runtime crashes.
- **Controlled Purity**: Minimize or eliminate `unsafePerformIO` in the error rendering pipeline to ensure the system remains fully predictable.

## 5. Logic Consistency
- **Order-Independent Configuration**: Ensure `remapMetadata` rules are applied globally regardless of their declaration order relative to `match` rules.
- **Strict Global Rules**: Audit `RuleGlobal` to ensure it does not bypass Shake's dependency tracking, preserving build determinism.

## 6. UX and UI Ease
- **Strict Validation Enforcement**: Update the CLI to return a **non-zero exit code** when Validator or SEO errors are detected, ensuring CI/CD safety.
- **Scaffold Templating**: Implement a flexible templating system for the `sara new` command to allow user-defined project structures.

## 7. Performance Benchmarking
- **Template Optimization**: Investigate pre-compilation strategies for Mustache templates to narrow the throughput gap with Go-based engines.
- **Metadata Memory Management**: Optimize the metadata tree representation to reduce the memory overhead of `Aeson.Value` for very large sites.
