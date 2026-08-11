# SARA Industrial Quickstart

Welcome to SARA (Simple, Adaptive, Responsive Architecture). SARA is a high-performance static site engine built for mission-critical security and industrial-scale correctness.

## 1. Installation
Ensure you have GHC (9.2+) and Cabal installed. 

```bash
git clone https://github.com/user/sara.git
cd sara
cabal install
```

## 2. Zero-Config Start
SARA can build a site without any configuration. It automatically detects your content and applies an industrial-grade pipeline.

```bash
# Create a new project with the modern "Wow" scaffold
sara new my-awesome-site
cd my-awesome-site

# Build the site instantly
sara build

# Start the dev server with Live Preview and Dashboard
sara serve
```

## 3. The Industrial Dashboard
When running `sara serve`, visit **`http://localhost:8080/sara`**. 
This is your **Command Center**, showing:
*   **Security Guard Status**: Ironclad verification of your path/shell security.
*   **SEO Health**: Real-time pass/fail on meta tags, titles, and alt text.
*   **Build Performance**: Detailed metrics on incremental throughput.

## 4. Key CLI Commands
*   `sara new`: Scaffolds a production-ready site with Search, SEO, and Dark Mode.
*   `sara build`: Executes the incremental build engine.
*   `sara serve`: Starts the dev server with targeted DOM patching (only refreshes the element you changed).
*   `sara check`: Runs a strict validation audit of all links and security guards.

---
**Next Step**: Read [The Industrial Deployment Guide](./DEPLOYMENT.md) to launch your site.
