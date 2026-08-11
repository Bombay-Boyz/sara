# Web Engineering Standard — HTML, CSS & JavaScript
### (No frameworks. No site builders. No CMS. Gold-standard vanilla web development.)

**Status: binding.** This document governs any HTML/CSS/JS built under it —
read before the first file is created, not retrofitted later. It applies
to human and AI authors alike.

**Precedence when principles conflict**, highest first:
1. **Works without CSS/JS** — content and navigation must survive both
   being absent or failing to load.
2. **Semantics & accessibility** — the correct element/attribute, used
   correctly, is never optional.
3. **Performance** — Core Web Vitals are a requirement, not an aspiration.
4. **SEO** — a byproduct of 2 and 3 done correctly, never a separate hack.
5. **Visual polish** — the last thing optimized for, never the first.

---

## Part 0 — Foundational Principles

**0.1 Progressive enhancement.** Build in layers: HTML that stands alone
→ CSS that enhances it → JS that enhances further. Each layer must degrade
gracefully if the next one fails or is disabled.

**0.2 Mobile-first.** Design and write CSS for the smallest viewport
first; add complexity upward via `min-width` media queries. Never design
for desktop and "make it responsive" afterward.

**0.3 Strict separation of concerns.** Structure lives in HTML,
presentation in CSS, behavior in JS. No inline `style=""`, no inline
`onclick=""`, no styling via JS when CSS can do it.

**0.4 One canonical stylesheet architecture.** A single, deliberately
ordered stylesheet (or an explicit, small import chain with a stated
order) — never multiple stylesheets silently fighting over specificity,
and never per-page ad hoc `<style>` blocks.

**0.5 Content survives first.** The page must be readable and navigable
with CSS disabled and JS disabled. This is the practical test of 0.1 —
if it fails, the layering is wrong somewhere.

**0.6 Vanilla by default.** No library or framework is added "just in
case." A dependency is added only when it removes meaningfully more
complexity than it introduces in bytes, maintenance, and audit surface —
for the scope this document targets (a marketing site, a documentation
site, a small business site, a blog), vanilla HTML/CSS/JS is almost
always sufficient.

**0.7 A performance budget is set before the first feature is built** —
e.g. total page weight, request count, and Lighthouse Performance score
targets — and every later feature is checked against it, not against
"does it feel fast on my machine."

**0.8 Accessibility (WCAG 2.1 AA) is a baseline requirement**, not an
afterthought applied to accommodate a specific complaint. See Part 7.

**0.9 SEO is a consequence of correct markup, not a bolt-on.** Semantic
HTML, real headings, descriptive links, and fast pages produce good SEO
as a side effect (Part 6); metadata is the finishing touch, not the
substitute.

**0.10 Design for real content and real devices**, not an idealized
375px mockup — test with long titles, missing images, slow 3G, and an
actual phone in your hand, not just a resized browser window.

---

## Part 1 — HTML: Semantic Structure

**1.1 Exactly one `<h1>` per page; headings in strict hierarchical
order** (`h1`→`h2`→`h3`, never skipping a level). The heading outline is
the page's table of contents, read by both screen readers and search
engines — treat it as load-bearing, not cosmetic.

**1.2 Use landmark elements for structural roles — never a `<div>`.**
`<header>`, `<nav>`, `<main>`, `<aside>`, `<footer>` are how assistive
technology and search engines understand page structure. There is
exactly one `<main>` per page.

**1.3 Use the most specific semantic element available**: `<article>`
for standalone content, `<section>` for a thematic grouping with its own
heading, `<figure>`/`<figcaption>` for images with captions, `<time
datetime="...">` for dates, `<address>` for contact info, `<blockquote
cite="...">` for quotations. A `<div>` or `<span>` is the fallback for
"none of these apply," not the default.

**1.4 Every `<img>` has meaningful `alt` text**; purely decorative
images get `alt=""` (not a missing attribute — an *empty* one, so
screen readers correctly skip it rather than reading the file name).

**1.5 Every form control has an associated `<label>`**, the correct
`type`/`inputmode`/`autocomplete` attribute, and related fields are
grouped with `<fieldset>`/`<legend>`. A placeholder is never a
substitute for a label.

**1.6 Breadcrumbs are both visible and structured.**

```html
<nav aria-label="Breadcrumb">
  <ol class="breadcrumb">
    <li><a href="/">Home</a></li>
    <li><a href="/guides/">Guides</a></li>
    <li aria-current="page">Choosing a Tent</li>
  </ol>
</nav>

<script type="application/ld+json">
{
  "@context": "https://schema.org",
  "@type": "BreadcrumbList",
  "itemListElement": [
    { "@type": "ListItem", "position": 1, "name": "Home", "item": "https://example.com/" },
    { "@type": "ListItem", "position": 2, "name": "Guides", "item": "https://example.com/guides/" },
    { "@type": "ListItem", "position": 3, "name": "Choosing a Tent" }
  ]
}
</script>
```

**1.7 A complete, correct meta block on every page** — this is the
minimum, adjusted per page for title/description/canonical/OG image:

```html
<meta charset="UTF-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>Choosing a Tent for Backpacking | Example Guides</title>
<meta name="description" content="A practical guide to choosing a backpacking tent by weight, season rating, and pitching style.">
<link rel="canonical" href="https://example.com/guides/choosing-a-tent/">
<link rel="icon" href="/favicon.ico" sizes="any">
<link rel="icon" href="/icon.svg" type="image/svg+xml">
<link rel="apple-touch-icon" href="/apple-touch-icon.png">

<!-- Open Graph -->
<meta property="og:type" content="article">
<meta property="og:title" content="Choosing a Tent for Backpacking">
<meta property="og:description" content="A practical guide to choosing a backpacking tent by weight, season rating, and pitching style.">
<meta property="og:image" content="https://example.com/images/tent-guide-og.jpg">
<meta property="og:url" content="https://example.com/guides/choosing-a-tent/">

<!-- Twitter Card -->
<meta name="twitter:card" content="summary_large_image">
```

**1.8 `lang` is set on `<html>`**, and any section in a different
language carries its own `lang` attribute — this affects both screen
reader pronunciation and search indexing.

**1.9 No empty interactive elements.** Every link and button has an
accessible name — visible text, or `aria-label` when an icon-only
control genuinely can't carry visible text.

**1.10 A skip-to-content link is the first focusable element on the
page** (`<a class="skip-link" href="#main">Skip to content</a>`),
visually hidden until focused, so keyboard users aren't forced through
the entire nav on every page.

**1.11 Valid HTML5, checked, not assumed** — the page passes the
[W3C Validator](https://validator.w3.org) with zero errors (Part 9).

---

## Part 2 — CSS: One Stylesheet, Disciplined Architecture

**2.1 A single canonical stylesheet** (or a small, explicitly ordered
`@import`/build-concatenation chain with the order documented at the
top of the entry file) is the only source of presentation. No
competing stylesheets, no per-page `<style>` blocks.

**2.2 Layer the stylesheet, in this order, and keep the layers visually
separated with comments:**

```css
/* 1. SETTINGS — design tokens, no visible output */
/* 2. GENERIC — reset/normalize */
/* 3. ELEMENTS — bare HTML element defaults (h1, p, a...) */
/* 4. OBJECTS — layout patterns (.o-container, .o-grid) */
/* 5. COMPONENTS — UI pieces (.c-card, .c-button) */
/* 6. UTILITIES — single-purpose overrides (.u-hidden, .u-mt-4) */
```
Later layers may override earlier ones; earlier layers never depend on
later ones. This ordering is what keeps specificity predictable as the
stylesheet grows, instead of an escalating war of `!important`.

**2.3 Design tokens are CSS custom properties, defined once, in `:root`
— the single source of truth for color, spacing, type scale, radii, and
breakpoints:**

```css
:root {
  --color-text: #1a1a1a;
  --color-bg: #ffffff;
  --color-primary: #0b5fff;
  --color-primary-contrast: #ffffff;

  --space-1: 0.25rem;
  --space-2: 0.5rem;
  --space-3: 1rem;
  --space-4: 2rem;

  --font-sans: system-ui, -apple-system, "Segoe UI", sans-serif;
  --step-0: clamp(1rem, 0.95rem + 0.25vw, 1.125rem);
  --step-2: clamp(1.5rem, 1.3rem + 1vw, 2.25rem);

  --radius-md: 0.5rem;
  --shadow-card: 0 1px 3px rgba(0, 0, 0, 0.12);
}

@media (prefers-color-scheme: dark) {
  :root {
    --color-text: #f2f2f2;
    --color-bg: #121212;
  }
}
```
A hardcoded color/spacing value appearing anywhere outside this block is
a defect — every visual property traces back to a named token.

**2.4 A disciplined naming convention (BEM or equivalent), low
specificity everywhere, zero `!important` outside true one-off utility
overrides:**

```css
/* Block, Element, Modifier — flat specificity, no nesting-driven cascades */
.c-card { }
.c-card__title { }
.c-card__title--featured { }
```
A selector nested more than two levels deep, or an ID used for styling,
is a sign the component boundary is wrong, not a reason to add
specificity to compensate.

**2.5 Media queries are mobile-first, ascending (`min-width`), tied to
tokenized breakpoints, never a magic number typed inline:**

```css
:root {
  --bp-tablet: 48rem;   /* 768px */
  --bp-desktop: 64rem;  /* 1024px */
}

.o-grid { display: grid; grid-template-columns: 1fr; gap: var(--space-3); }

@media (min-width: 48rem) {
  .o-grid { grid-template-columns: repeat(2, 1fr); }
}
@media (min-width: 64rem) {
  .o-grid { grid-template-columns: repeat(3, 1fr); }
}
```

**2.6 Fluid, relative units — not fixed pixels for layout or type.**
`rem` for type and spacing (scales with user font-size preference), `%`/
`fr`/`clamp()` for layout, `px` reserved for things that genuinely should
never scale (a 1px hairline border).

```css
h1 { font-size: clamp(1.75rem, 1.4rem + 1.5vw, 3rem); }
```

**2.7 Modern layout — Flexbox and Grid, never floats or absolute-
positioning hacks for layout.** `display: grid`/`display: flex` plus
`gap` replaces margin-based spacing hacks between siblings.

**2.8 User preferences are respected:**
```css
@media (prefers-reduced-motion: reduce) {
  *, *::before, *::after {
    animation-duration: 0.001ms !important;
    animation-iteration-count: 1 !important;
    transition-duration: 0.001ms !important;
  }
}
```
Dark mode follows `prefers-color-scheme` (2.3) by default; an explicit
in-page toggle is additive, not a replacement for respecting the OS
preference.

**2.9 A print stylesheet is included** (`@media print`) — hide
navigation/decorative elements, expand link URLs if relevant, ensure
body text remains legible in black-and-white.

**2.10 No inline `style=""` attributes and no per-page `<style>`
blocks** — every visual rule lives in the canonical stylesheet (2.1),
findable in one place.

**2.11 Container queries for component-level responsiveness** — when a
component's layout should depend on the space *it* has, not the
viewport (a card that reflows differently in a sidebar vs. full-width):

```css
.c-card-wrapper { container-type: inline-size; }

@container (min-width: 30rem) {
  .c-card { display: grid; grid-template-columns: auto 1fr; }
}
```

---

## Part 3 — JavaScript: Vanilla, Unobtrusive, Progressive

**3.1 Core content and navigation work with JS disabled** (0.5). JS
*enhances* — a mobile nav toggle, a live form validation message, a
carousel — it never gates access to primary content or navigation.

**3.2 No inline event handlers in HTML.** No `onclick=""`,
`onsubmit=""`. Listeners are attached in JS, keeping behavior in one
place instead of scattered across markup.

```html
<!-- Bad -->
<button onclick="toggleMenu()">Menu</button>

<!-- Good -->
<button id="menu-toggle" aria-expanded="false" aria-controls="menu">Menu</button>
```
```js
document.getElementById('menu-toggle').addEventListener('click', toggleMenu);
```

**3.3 Scripts never block rendering.** `<script src="/js/main.js" defer></script>`
(or `type="module"`, deferred by default) at the end of `<head>` or just
before `</body>`; nothing render-blocking.

**3.4 ES modules; no global namespace pollution.** One `<script
type="module">` entry point, everything else imported — no dozen
loose `<script>` tags each defining global functions.

**3.5 Event delegation over many individual listeners** — attach one
listener to a stable ancestor and inspect `event.target`, rather than
binding a listener to every item in a list that might change.

```js
document.querySelector('.c-list').addEventListener('click', (event) => {
  const item = event.target.closest('.c-list__item');
  if (item) handleItemClick(item);
});
```

**3.6 `IntersectionObserver` for scroll-triggered behavior and lazy-
loading** — never a `scroll` event handler polling `getBoundingClientRect`
on every frame.

**3.7 Defensive by default** — feature-detect before using a newer API,
and handle every `fetch`/network failure explicitly:

```js
async function loadData(url) {
  try {
    const res = await fetch(url);
    if (!res.ok) throw new Error(`Request failed: ${res.status}`);
    return await res.json();
  } catch (err) {
    console.error('loadData failed', err);
    showUserFacingError();
    return null;
  }
}
```

**3.8 No framework for simple DOM tasks.** `querySelector`,
`classList`, native `fetch`, and template literals are sufficient for
the scope this document targets — a UI library earns its place only
when the interaction complexity genuinely requires it (0.6).

**3.9 Custom widgets are fully accessible.** A component built from
`<div>`s (a tab set, a modal, a disclosure) implements the matching
ARIA pattern correctly (`role`, `aria-expanded`, `aria-selected`,
`aria-controls`), full keyboard support (Tab/Enter/Space/Arrow keys as
appropriate), and manages focus explicitly (e.g. moving focus into a
modal on open, back to the trigger on close).

**3.10 No `eval`; no `innerHTML` with untrusted or user-supplied
content** — use `textContent` for plain text, and sanitize explicitly
(or build nodes with `createElement`) for anything that must include
markup. Untreated `innerHTML` of user input is a stored-XSS vector,
covered further in Part 10.

---

## Part 4 — Performance (Core Web Vitals)

**4.1 Target thresholds, treated as gates (Part 9):** Largest
Contentful Paint (LCP) < 2.5s, Interaction to Next Paint (INP) < 200ms,
Cumulative Layout Shift (CLS) < 0.1.

**4.2 Every image and embed has explicit dimensions** — `width`/`height`
attributes (or `aspect-ratio` in CSS) so the browser reserves space
before the asset loads, directly preventing CLS.

```html
<img src="/images/tent.jpg" alt="A two-person tent pitched on a ridge"
     width="1200" height="800" loading="lazy">
```

**4.3 Responsive images, modern formats with fallback:**

```html
<picture>
  <source type="image/avif" srcset="/images/tent-400.avif 400w, /images/tent-800.avif 800w" sizes="(min-width: 48rem) 50vw, 100vw">
  <source type="image/webp" srcset="/images/tent-400.webp 400w, /images/tent-800.webp 800w" sizes="(min-width: 48rem) 50vw, 100vw">
  <img src="/images/tent-800.jpg" alt="A two-person tent pitched on a ridge" width="800" height="533" loading="lazy">
</picture>
```

**4.4 Lazy-load everything below the fold; load the LCP element
eagerly.** `loading="lazy"` on offscreen images/iframes; the hero/LCP
image is `loading="eager"` (or the attribute simply omitted) and, if it
is the LCP candidate, preloaded (4.6).

**4.5 Fonts are deliberate, not default.** `font-display: swap`; the
critical font file is preloaded; the font set is subset to the
characters actually used; the number of families/weights loaded is kept
to the minimum the design needs.

```html
<link rel="preload" href="/fonts/inter-var.woff2" as="font" type="font/woff2" crossorigin>
```

**4.6 Resource hints used deliberately:**
```html
<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
<link rel="dns-prefetch" href="https://www.google-analytics.com">
<link rel="preload" href="/images/hero.avif" as="image">
```
`preconnect`/`dns-prefetch` for third-party origins that will definitely
be used; `preload` only for the specific asset that's actually on the
critical rendering path — over-using `preload` competes with genuinely
critical resources for bandwidth.

**4.7 Production assets are minified and compressed** (gzip or brotli
at the server/CDN level) and static files are named with a content
hash (`main.a1b2c3.css`) so they can be cached forever and safely
invalidated on change.

**4.8 Cache headers reflect asset lifetime**: far-future
`Cache-Control: public, max-age=31536000, immutable` on hashed static
assets; short or `no-cache` on the HTML documents that reference them.

**4.9 CSS is not a rendering bottleneck** — the single stylesheet (2.1)
stays small enough in practice that it isn't the long pole for first
paint; if it grows large, the truly critical above-the-fold rules are
inlined and the rest loaded async, rather than splintering into
multiple competing stylesheets (which 2.1 already prohibits).

**4.10 Every third-party script is a deliberate, audited cost** —
analytics, chat widgets, embeds each add weight, block the main thread,
and are a privacy/security surface. Each one is justified individually,
loaded `async`/`defer`, and periodically re-audited for whether it's
still earning its place.

---

## Part 5 — Responsive Design (Mobile / Tablet / Desktop)

**5.1 Breakpoints are tokens (2.3, 2.5), defined once**, never a magic
number typed at each media query — changing a breakpoint should mean
changing one variable, not grepping the whole stylesheet.

**5.2 The viewport meta tag is correct and unmodified:**
```html
<meta name="viewport" content="width=device-width, initial-scale=1">
```
Never disable zoom (`user-scalable=no`, `maximum-scale=1`) — that's an
accessibility violation for anyone who needs to zoom.

**5.3 Touch targets are at least 44×44px** (WCAG 2.5.5) with enough
surrounding space that adjacent targets aren't mis-tapped.

**5.4 Fluid typography via `clamp()`** (2.6) instead of a font-size
that jumps discretely at each breakpoint.

**5.5 Test at, minimum, four real widths**: ~375px (mobile),
~768px (tablet), ~1280px (laptop/desktop), ~1920px+ (wide desktop) —
on an actual device or an accurate emulator, not just a resized desktop
browser window, since real devices differ in touch behavior, font
rendering, and network conditions.

**5.6 No horizontal scroll at any supported width.** No fixed-width
containers (`width: 960px`) — containers are `max-width` with fluid
inner content (`width: 100%`, `max-width: 75rem`, centered).

**5.7 Media scales fluidly**: `img, video, iframe { max-width: 100%;
height: auto; }` as a baseline rule in the elements layer (2.2), with
explicit `aspect-ratio` (4.2) preserved.

---

## Part 6 — SEO

**6.1 One unique, descriptive `<title>` per page**, roughly 50–60
characters, front-loaded with the specific topic, not just the site
name.

**6.2 One unique `meta description` per page**, roughly 150–160
characters, written as genuine ad copy for that specific page — never
duplicated across pages, never auto-generated boilerplate.

**6.3 A canonical URL on every page** (1.7) — critical for any content
reachable via more than one URL (with/without trailing slash, query
parameters, etc.), to avoid duplicate-content penalties.

**6.4 Open Graph and Twitter Card metadata on every shareable page**
(1.7) — controls how the page appears when shared, which is itself an
SEO/traffic factor, not just a cosmetic one.

**6.5 Structured data (JSON-LD) matching the page's actual content
type** — `BreadcrumbList` (1.6) on every page with a hierarchy,
`Article`/`Product`/`Organization`/`WebSite`/`FAQPage` as applicable.
Structured data must accurately describe what's genuinely on the page —
never markup for content the page doesn't actually contain.

**6.6 Clean, descriptive, human-readable URLs**:
`/guides/choosing-a-tent/`, not `/index.php?id=482&cat=3`. Hyphens
separate words; no session IDs or unnecessary query strings in
canonical URLs.

**6.7 `sitemap.xml` and `robots.txt` are maintained and accurate** —
every canonical, indexable page is in the sitemap; nothing accidentally
disallowed that should be indexed.

**6.8 Internal links use descriptive anchor text** — "See our guide to
[choosing a tent]" rather than "[click here]"; anchor text is itself an
SEO and accessibility signal simultaneously.

**6.9 The heading outline (1.1) doubles as SEO structure** — the `h1`
states the page's actual topic in the way a searcher would phrase it;
`h2`s are the page's real sections, not decoratively-sized paragraph
text.

**6.10 Page speed and mobile-friendliness are ranking factors, not a
separate checklist** — full compliance with Parts 4 and 5 *is* an SEO
requirement, already covered, not something to redo here.

---

## Part 7 — Accessibility (WCAG 2.1 AA Baseline)

**7.1 Color contrast** ≥ 4.5:1 for normal text, ≥ 3:1 for large text
(18pt+/14pt+ bold) and meaningful UI components/icons — checked with a
contrast tool against the actual token values (2.3), not eyeballed.

**7.2 Full keyboard operability.** Every interactive element is
reachable via Tab and operable via Enter/Space (or arrow keys, per the
correct ARIA pattern — 3.9); a visible focus indicator is never removed
(`outline: none` without a replacement is a defect, not a style choice).

**7.3 ARIA fills gaps semantic HTML can't express — it's not a default
layer.** A native `<button>` doesn't need `role="button"`; adding ARIA
to an element that already has the correct native semantics is
redundant at best and can override correct behavior at worst. Use the
first rule of ARIA: if a native HTML element or attribute has the
semantics you need, use it instead of adding ARIA.

**7.4 Form errors are announced and associated with their field** —
`aria-describedby` linking the error message to the input, and never
conveyed by color alone (a red border with no text is invisible to a
screen reader and to anyone with color-vision deficiency).

**7.5 Motion respects `prefers-reduced-motion`** (2.8) — this is a
strict requirement, not a nice-to-have, for anyone with vestibular
disorders.

**7.6 Skip link (1.10) plus a focus order that matches the visual
order** — tabbing through the page should never jump somewhere
visually unexpected.

---

## Part 8 — Maintainability & Project Structure

**8.1 A predictable folder structure:**
```
/
├── index.html
├── /guides/choosing-a-tent/index.html
├── /css/main.css
├── /js/main.js
├── /images/
├── /fonts/
├── sitemap.xml
└── robots.txt
```

**8.2 One naming convention, consistently, across HTML classes, JS
identifiers, and file names** — kebab-case for files and CSS classes
(2.4), camelCase for JS variables/functions, no mixing conventions
within one category.

**8.3 No inline styles/scripts (2.10, 3.2); no magic numbers** — a
spacing value, breakpoint, color, or timing duration is a named token
or constant (2.3), never a bare literal repeated across files.

**8.4 Comments state *why*, not *what*** — `/* 3-column on desktop to
match the print catalog layout */`, not `/* make it three columns */`.

**8.5 Linting enforced as a CI gate, not a suggestion**: `markuplint` or
`HTMLHint` for HTML, `Stylelint` for CSS, `ESLint` for JS — configured
to fail the build, mirroring the discipline in the companion Haskell
standard's Part 5.

**8.6 Formatting enforced via Prettier** (or equivalent) in CI, so
review diffs are never mixed style-noise and substance.

**8.7 Static assets are versioned/cache-busted on every deploy**
(content-hashed filenames, 4.7) so a deploy is never blocked by, or
silently served from, a stale cache.

**8.8 The supported browser matrix is written down explicitly** (e.g.
"last 2 versions of Chrome, Firefox, Safari, Edge; iOS Safari 15+") and
tested against — not assumed.

---

## Part 9 — Testing & Verification (CI Gates)

**9.1 W3C HTML validation** — zero errors, required to merge.

**9.2 Lighthouse CI** — Performance, Accessibility, Best Practices, and
SEO scores each above an agreed threshold (e.g. 90+); a regression
below threshold fails the build, the same way a benchmark regression
does in the companion Haskell standard (5.6).

**9.3 Automated accessibility audit** (`axe-core` or equivalent) — zero
critical/serious violations, required to merge.

**9.4 Broken-link checking** across the whole built site — zero 404s
from internal links, required to merge.

**9.5 Cross-browser smoke test** on the core pages against the
documented browser matrix (8.8) before each release.

**9.6 A performance budget is enforced mechanically** — maximum total
page weight, maximum request count, maximum JS bundle size — the build
fails if a change exceeds the budget set in 0.7, the same way the
Haskell standard treats a benchmark regression as a failing test.

---

## Part 10 — Security

**10.1 HTTPS everywhere, with HSTS enabled** — no page or asset served
over plain HTTP, no mixed content.

**10.2 A Content-Security-Policy header** restricting script and style
sources to trusted origins, reducing the blast radius of any injected
content.

**10.3 Subresource Integrity (SRI)** on any third-party CDN-hosted
script or stylesheet:
```html
<script src="https://cdn.example.com/lib.js"
        integrity="sha384-..." crossorigin="anonymous"></script>
```

**10.4 All user-supplied content is sanitized/escaped before rendering**
— `textContent`, not `innerHTML`, for anything derived from user input
(3.10); if HTML must be inserted, sanitize explicitly (e.g. DOMPurify)
first.

**10.5 No secrets or API keys in client-side JS** — anything shipped to
the browser is public by definition; secrets stay server-side.

---

## Pre-Launch Checklist

- [ ] Page works with CSS disabled and with JS disabled (0.5, 3.1)
- [ ] Exactly one `<h1>`; heading order has no skipped levels (1.1)
- [ ] Landmarks used correctly; exactly one `<main>` (1.2)
- [ ] Every image has correct `alt` (meaningful or `alt=""`) (1.4)
- [ ] Every form control has a real `<label>` (1.5)
- [ ] Breadcrumb nav + `BreadcrumbList` JSON-LD present (1.6)
- [ ] Full meta block present and page-specific: title, description,
      canonical, OG, Twitter Card, favicon (1.7, 6.1–6.4)
- [ ] `lang` set correctly (1.8)
- [ ] Skip link present and functional (1.10)
- [ ] Zero W3C HTML validation errors (1.11, 9.1)
- [ ] All styling in the single canonical stylesheet; zero inline
      styles or per-page `<style>` blocks (2.1, 2.10)
- [ ] All design values traced to tokens; zero hardcoded colors/spacing
      (2.3)
- [ ] Zero `!important` outside true utility overrides (2.4)
- [ ] Mobile-first media queries only, tied to breakpoint tokens (2.5, 5.1)
- [ ] `prefers-color-scheme` and `prefers-reduced-motion` respected
      (2.8, 7.5)
- [ ] Print stylesheet present (2.9)
- [ ] No inline event handlers; all JS listeners attached in script
      files (3.2)
- [ ] Scripts `defer`/`type="module"`; nothing render-blocking (3.3)
- [ ] Every image/embed has explicit dimensions or `aspect-ratio` (4.2)
- [ ] Responsive images with modern formats + fallback in place (4.3)
- [ ] LCP resource identified and preloaded; everything else below the
      fold lazy-loaded (4.4, 4.6)
- [ ] Lighthouse Performance/Accessibility/Best Practices/SEO all above
      threshold (4.1, 9.2)
- [ ] Zero horizontal scroll at any tested width (5.6)
- [ ] Touch targets ≥ 44×44px (5.3)
- [ ] Contrast ratios verified against actual tokens (7.1)
- [ ] Full keyboard operability verified manually (7.2)
- [ ] `sitemap.xml` and `robots.txt` accurate and current (6.7)
- [ ] Zero critical/serious `axe-core` violations (9.3)
- [ ] Zero broken internal links (9.4)
- [ ] HTTPS + HSTS enabled; CSP header set; SRI on third-party scripts
      (10.1–10.3)
- [ ] No secrets in client-side JS (10.5)
