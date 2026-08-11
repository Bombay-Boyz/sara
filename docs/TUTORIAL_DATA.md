# Tutorial: Ingesting External Data

Industrial sites often rely on external data like product catalogs, team members, or project lists stored in JSON or YAML. SARA makes this easy with `loadData`.

## 1. Local Data Files
Suppose you have a file `data/team.yaml`:

```yaml
- name: Alice
  role: Architect
- name: Bob
  role: Security Lead
```

## 2. Ingesting in your DSL
You can load this data directly into your `site.hs` main loop.

```haskell
main = sara $ do
  -- SARA automatically tracks this file for changes
  teamData <- loadData "data/team.yaml"

  -- You can now pass this data to any template or use it in logic
  match (glob "index.html") $ \file -> do
     item <- readMarkdown file
     -- Merge team data into the page metadata
     let meta = itemMeta item <> [("team", teamData)]
     render "templates/index.html" item { itemMeta = meta }
```

## 3. Supported Formats
*   **JSON**: `.json` extension.
*   **YAML**: `.yaml` or `.yml` extensions.
*   **Automatic Dependency Tracking**: If you edit `team.yaml`, SARA instantly re-renders any page that depends on it.

---
**Next Step**: Read about the [Technical Architecture](./ARCHITECTURE.md).
