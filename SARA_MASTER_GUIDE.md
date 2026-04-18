# SARA: Simple, Adaptive, Responsive Architecture
## The Master Guide to High-Assurance Static Site Generation

> *"SARA is a commitment to the idea that industrial software should be incredibly fast, welcoming to newcomers, and a joy to maintain."*

---

## 1. The SARA Mission

SARA is a high-performance, developer-centric static site generator (SSG) built in Haskell. Our goal is to provide the **Type-Safe elegance of Haskell** with the **Raw Performance of Rust** (Zola/Hugo), packaged in a "batteries-included" framework that respects the developer's time and eases the transition from legacy systems.

### Why SARA?
- **Auto-Discovery:** No more manual boilerplate for assets or CSS.
- **Instant Feedback:** Integrated WebSockets for live browser updates.
- **Unified Data Model:** Everything is JSON (Aeson), making metadata intuitive.
- **Human-First Errors:** Diagnostics that pinpoint the exact line and column of a failure.
- **Ironclad Security:** Built-in protection against path traversal, ReDoS, and shell injection.

---

## 2. Industrial Architecture

SARA is designed using high-assurance Haskell patterns that ensure correctness and massive scale.

### 2.1 The Monad Stack: `SaraM`
SARA uses the **RIO Pattern** for state management:
- **Type:** `ReaderT SaraEnv IO`
- **Capability:** `MonadUnliftIO`
- **Why?** This ensures that even in highly concurrent builds (multicore), the state of the build remains consistent and resource acquisition (like file handles) is safe from leaks.

### 2.2 Unified State: `SaraState`
Instead of fragmented mutable variables, all build state is consolidated into a single atomic record:
- **Site Graph:** Tracks all generated URLs to prevent broken internal links.
- **Item Cache:** In-memory storage of processed pages for blistering speed.
- **Plugin Registry:** Dynamic map of custom shortcode handlers.
- **Dependency Tracker:** Automated registration of every file read during the build.

### 2.3 Performance standard: `SPath`
SARA eliminates the "Beginner 101" mistake of using linked-list `String`s for paths. Every path in SARA is represented by **`SPath` (Text)**, reducing memory allocations by up to 80% on large-scale sites.

---

## 3. The SARA DSL (API Reference)

The Domain Specific Language (DSL) is how you describe your site.

### Core Functions
- `match :: GlobPattern -> (SPath -> SaraM (Item 'Validated)) -> SaraM [Item 'Validated]`
  Finds files matching a pattern and runs your logic on each.
- `readMarkdown :: SPath -> SaraM (Item 'Unvalidated)`
  Reads a Markdown file, extracts metadata, and prepares it for rendering.
- `validateSEO :: Item 'Unvalidated -> SaraM (Item 'Validated)`
  Checks for missing titles, descriptions, and alt tags. Returns a warning if description is missing, but fails on missing titles.
- `render :: SPath -> Item 'Validated -> SaraM ()`
  Applies a Mustache template to an item and writes the output.
- `registerShortcode :: Text -> (Shortcode -> SaraM Text) -> SaraM ()`
  Registers a custom dynamic plugin (e.g., `{{% youtube id="..." %}}`).

---

## 4. Beginner Tutorial: From Zero to Site

This tutorial will guide you through creating your first SARA site in 5 minutes.

### Step 1: Install SARA
Ensure you have `ghcup` installed. Clone the repository and install the binary:
```bash
cabal install
```

### Step 2: Initialize a Project
Create a new directory and scaffold a basic project:
```bash
mkdir my-blog
cd my-blog
sara init
```
This creates:
- `sara.yaml`: Your site configuration.
- `site.hs`: Your build logic (The "Brain").
- `posts/`: Where your Markdown files live.
- `templates/`: Your HTML layouts.

### Step 3: Write Your First Post
Create `posts/hello.md`:
```markdown
---
title: My First Post
description: Welcome to my SARA site!
author: Your Name
---
# Hello World
This is a post rendered by SARA.
```

### Step 4: Configure the Build
Open `site.hs`. It will look something like this:
```haskell
import SARA

main :: IO ()
main = sara $ do
  -- 1. Copy static assets
  discover (glob "assets/*")

  -- 2. Process Markdown posts
  items <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    item' <- validateSEO item
    render "templates/post.html" item'
    return item'

  -- 3. Generate a sitemap for Google
  buildSitemap "sitemap.xml" items
```

### Step 5: Build and Serve
Run the live development server:
```bash
sara serve
```
Open `http://localhost:8000` in your browser. Any change you make to your Markdown or CSS will instantly reflect in the browser without a refresh!

---

## 5. Security & Safety

SARA is built for the hostile open web. It includes three "Guards" that run automatically:

1. **PathGuard:** Prevents `../` traversal attacks. If a template tries to read a file outside your project root, the build fails.
2. **ShellGuard:** Sanitizes all arguments passed to external tools (like ImageMagick). It is immune to `; rm -rf /` injection.
3. **RegexGuard:** Checks your routing regexes for "Nested Quantifiers" that could cause ReDoS (Regular Expression Denial of Service) attacks.

---

## 6. Advanced: Dynamic Plugins

You can extend SARA's Markdown engine with monadic shortcodes.

**Example: A "Gist" Shortcode**
```haskell
registerShortcode "gist" $ \sc -> do
  let id = Map.findWithDefault "" "id" (scArgs sc)
  return $ "<script src=\"https://gist.github.com/" <> id <> ".js\"></script>"
```
Now use it in your Markdown: `{{% gist id="abc123" %}}`.

---

## 7. Troubleshooting

| Error Code | Meaning | Fix |
| :--- | :--- | :--- |
| **E030** | Template Not Found | Check if the path in `render` matches a file in `templates/`. |
| **W003** | Missing Title | Add `title: ...` to your Markdown frontmatter. |
| **S001** | Path Traversal | Your build script tried to access a file outside the project root. |

---

> SARA: Built with love, optimized for the professional, accessible to all.
