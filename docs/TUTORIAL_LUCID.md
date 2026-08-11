# Tutorial: Type-Safe HTML with Lucid

One of SARA's most powerful features is the ability to bypass `.html` files entirely and write your UI in type-safe Haskell using the **Lucid** library. This ensures your templates are checked by the compiler.

## 1. Setup your `site.hs`
Instead of using the default Mustache renderer, we will use `renderWith`.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import SARA
import Lucid

main :: IO ()
main = sara $ do
  posts <- match (glob "posts/*.md") $ \file -> do
    item <- readMarkdown file
    item' <- validateSEO item
    -- Use Lucid instead of render "templates/post.html"
    renderWith myLucidTemplate item'
    pure item'
```

## 2. Defining the Template
Write your layout as a standard Haskell function.

```haskell
myLucidTemplate :: Item 'Validated -> Text
myLucidTemplate item = renderLucid (layout item) item

layout :: Item 'Validated -> Html ()
layout item = html_ $ do
  head_ $ do
    title_ (toHtml $ itemTitle item) -- Type-safe metadata access
    style_ "body { font-family: sans-serif; }"
  body_ $ do
    nav_ $ a_ [href_ "/"] "Home"
    main_ $ do
      h1_ (toHtml $ itemTitle item)
      div_ [class_ "content"] $ do
        toHtmlRaw (itemBody item) -- Raw HTML injection for Markdown
```

## 3. Why use Lucid?
1.  **Zero Runtime Errors**: If you misspell a tag or omit a closing bracket, your site won't compile.
2.  **Logic Power**: Use standard Haskell `if`, `case`, and `map` inside your HTML logic.
3.  **Performance**: Lucid is significantly faster than parsing Mustache templates from disk.

---
**Next Step**: Learn about [Ingesting External Data](./TUTORIAL_DATA.md).
