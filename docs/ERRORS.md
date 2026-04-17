# SARA Error Reference

## Frontmatter Errors (E001-E009)
- **E001 (FrontmatterUnknownFormat)**: The file starts with a known separator (`---`, `+++`) but the content is invalid.
- **E002 (FrontmatterParseFailure)**: Error parsing YAML/TOML/JSON content. Check syntax.
- **E003 (FrontmatterRemapMissing)**: A `remapMetadata` rule was specified but the source key was not found in the file.

## Security Errors (S001-S009)
- **S001 (SecurityPathTraversal)**: Attempt to access a file outside the project root.
- **S002 (SecurityGlobEscape)**: Glob pattern contains `..` or absolute paths.
- **S003 (SecurityRegexReDoS)**: Regular expression is too complex and could cause a denial-of-service.
- **S005 (SecurityUnsafeTemplate)**: A template uses `{{{ }}}` raw interpolation for a key that isn't whitelisted.

## SEO Warnings (W001-W009)
- **W001 (SEOAltMissing)**: `<img>` tag is missing the `alt` attribute.
- **W002 (SEOHeadingSkip)**: Heading levels skip a rank (e.g., `h1` followed by `h3`).
- **W003 (SEOTitleMissing)**: The rendered page has no `<title>` tag.

## Validator Errors (V001-V009)
- **V001 (ValidatorBrokenLink)**: An internal link points to a path that does not exist in the site graph.
