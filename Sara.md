SARA: Simple, Adaptive, Responsive Architecture

> **The Industrial-Grade Haskell Static Site Engine**

1. Mission & Vision

**SARA** is a high-performance, developer-centric static site generator. Named as a tribute to a source of inspiration and grace, SARA moves beyond the "static" limitations of its predecessors. It is designed to be **Simple** to configure, **Adaptive** to any data format, and **Responsive** to the developer’s workflow through instant feedback and blistering speed.

Our goal is to provide the **Type-Safe elegance of Haskell** with the **Raw Performance of Rust** (Zola/Hugo), packaged in a "batteries-included" framework that respects the developer's time and eases the transition from legacy systems.

------

2. The Need: Why SARA?

While Hakyll defined the "site-as-a-compiler" era, SARA addresses its modern shortcomings:

- **Boilerplate Fatigue:** No more manual rules for every CSS file or favicon. SARA uses **Auto-Discovery**.
- **Slow Feedback Loops:** SARA replaces manual refreshes with **Integrated WebSockets** for instant browser updates.
- **Complex Metadata:** SARA replaces custom `Context` types with a **Unified Aeson (JSON) Data Model**.
- **The Error Gap:** SARA replaces 50-line GHC backtraces with **Human-First Error Messages** that pinpoint the exact line in your Markdown.

------

3. "Migration-First" Philosophy

SARA is the only Haskell SSG designed to "adopt" existing sites from Jekyll, Hugo, or Hakyll with zero friction.

- **Universal Frontmatter Parser:** Automatically detects and merges YAML, TOML, or JSON metadata on a per-file basis.
- **URL Preservation Engine:** Use Regex-based route mirroring to maintain 1:1 parity with your old site's URL structure (e.g., `/year/month/day/title.html`).
- **Metadata Remapping:** Rename legacy fields (like `published_at` to `date`) via a simple configuration map without editing original files.
- **Shortcode Translation:** Built-in compatibility layers for common non-standard syntax like Jekyll’s `{% post_url %}`.
- **`sara import` CLI:** A specialized tool to scaffold a SARA project directly from an existing directory of Markdown and assets.

------

4. High-Performance Architecture

To achieve **Rust-level performance**, SARA leverages the best of modern Haskell:

- **Core Engine:** Built on **Shake**, providing a demand-driven, parallelized build graph.
- **Fast Hashing:** Uses **BLAKE3** content hashing to ensure incremental rebuilds happen in milliseconds.
- **Multicore Scaling:** Automatically saturates all CPU cores for heavy tasks like image processing.
- **Zero-Copy IO:** Optimized handling of `ByteString` and `Text` for minimal memory allocation.

------

5. "Batteries-Included" Advanced Features

**Built-in SEO Intelligence**

- **Automated JSON-LD:** Generates Schema.org structured data automatically from your Aeson metadata.
- **Social Graph Auto-Gen:** Automatically creates OpenGraph and Twitter Card tags.
- **Audit Engine:** Warns or fails the build if `alt` tags are missing or heading hierarchies (`h1`, `h2`) are broken.

**Industrial Link & Asset Validator**

- **Strict Internal Linking:** SARA scans every link during the build. If a link points to a non-existent internal page, the build **fails immediately** with a clear error.
- **Broken Image Prevention:** Ensures every `src` attribute maps to a valid asset in the build graph.

**Asset Powerhouse**

- **Native Image Processing:** Parallelized resizing and conversion to modern formats (WebP/Avif).
- **Search Indexing:** Automatically generates a client-side search index (compatible with Pagefind) during the build.
- **Shortcode System:** Simple `{{ youtube id="..." }}` style components that work out of the box.

------

6. Quality Assurance & The "SARA Standard"

SARA is built for **Long-Term Durability** through extreme testing:

- **QuickCheck Stress Testing:** Property-based testing to verify routing logic and metadata merging against thousands of "malformed" edge cases.
- **Industrial Benchmarking:** Every release must pass the **"10k Post Test"**:
  - **Cold Start:** < 2 seconds for 5,000 files.
  - **Hot Rebuild:** < 50ms for a single file change.
- **Memory Discipline:** Constant-space memory usage even when processing large image libraries.

------

7. Project Structure & Roadmap

**README & Tutorials**

- **The 60-Second Start:** `stack new my-site sara-template` or `cabal init`.
- **Migration Guides:** Dedicated "Switching from Hakyll/Jekyll" documentation.
- **The "Human" Error Guide:** Visual examples of SARA's colorized error reporting.

**Example: The SARA DSL**

haskell

```
import SARA

main = sara $ do
    -- 1. Migration: Treat old Jekyll metadata as standard fields
    remapMetadata [("published_at", "date")]

    -- 2. Simple: Auto-discover static assets
    discover "assets/*" 
    
    -- 3. Adaptive: Handle Markdown with SEO Validation & URL Preservation
    posts <- match "posts/*.md" $ \file -> do
        item <- readMarkdown file
        validateSEO item
        -- Keep original URL structure
        route (regexRoute "/posts/([0-9]{4})/(.*)" "/archives/\\1/\\2") 
        render "templates/post.html" item

    -- 4. Responsive: Index with internal link checking
    match "index.html" $ \file -> do
        render "templates/index.html" (object ["posts" .= posts])
```

Use code with caution.



------

**SARA** is more than a tool; it is a commitment to the idea that industrial software should be incredibly fast, welcoming to newcomers, and a joy to maintain.
