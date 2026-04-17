# Migrating to SARA

SARA is designed to be a replacement for legacy static site generators.

## From Jekyll
1. **Shortcodes**: SARA automatically translates `{% post_url %}`, `{% link %}`, and `{% highlight %}` during build.
2. **Structure**: Run `sara import .` in your Jekyll root. SARA will detect the structure and configure the build.
3. **Data**: Move `_data` to `assets/` or use `remapMetadata` in `site.hs`.

## From Hugo
1. **Shortcodes**: SARA supports common Hugo shortcodes like `{{< ref >}}` and `{{< highlight >}}`.
2. **Content**: Hugo's `content/` directory should be mapped using `match (glob "content/**/*.md")` in your `site.hs`.

## From Hakyll
1. **DSL**: SARA's DSL is very similar to Hakyll's. Most `match` and `route` logic can be copied with minor import changes.
2. **Configuration**: Use `sara.yaml` for site-wide settings.
