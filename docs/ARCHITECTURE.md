# SARA Technical Architecture

SARA is built on a "Verification-First" architecture. Every transformation of an item is indexed by a type-level state to ensure that unvalidated or insecure content never reaches the final site output.

## 1. The GADT-Indexed Pipeline
SARA uses a phantom type to track the `ValidationState` of an item.

```haskell
data ValidationState = Unvalidated | Validated

data Item (v :: ValidationState) = Item { ... }
```

The DSL ensures that `render` can only be called on an `Item 'Validated`. This prevents "silent failures" where a page with broken links or invalid SEO is published.

## 2. Dependency Graph (Shake)
The core engine is powered by **Shake**. Every major industrial feature is a node in this graph:
*   **LQIP Generation**: Cached via Shake Oracles.
*   **Inverted Indexing**: Incrementalized using partial JSON fragments.
*   **Template Rendering**: Tracked via `addTemplateOracle`.

## 3. Industrial Security Guards
*   **PathGuard**: A purely structural check that prevents directory traversal. It does not require the file system to exist, making it fast and testable.
*   **ShellGuard**: Sanitizes every argument passed to external image processing tools (like ImageMagick).

## 4. Incremental Search
Unlike other engines that re-build the search index from scratch, SARA:
1.  Generates a `.partial.json` for every changed page.
2.  Uses a "Global Merging" step to aggregate these partials into a final trie-based inverted index.
3.  This results in O(1) index updates during development.

## 5. LiveReload & DOM Patching
The dev server tracks client-side paths. When you edit a file, SARA computes the specific DOM patch and sends it only to browsers viewing that path, ensuring high performance even for massive documentation sets.
