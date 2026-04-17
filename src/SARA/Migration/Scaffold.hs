{-# LANGUAGE OverloadedStrings #-}

module SARA.Migration.Scaffold
  ( ScaffoldOptions(..)
  , scaffoldProject
  , scaffoldFromTemplate
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (createDirectoryIfMissing, copyFile, listDirectory, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad (forM_)

data ScaffoldOptions = ScaffoldOptions
  { scTitle   :: !Text
  , scAuthor  :: !Text
  , scBaseUrl :: !Text
  } deriving (Show)

-- | Scaffolds a new SARA project using hardcoded defaults.
scaffoldProject :: FilePath -> ScaffoldOptions -> IO ()
scaffoldProject root opts = do
  putStrLn $ "Scaffolding SARA project in " ++ root
  createDirectoryIfMissing True (root </> "posts")
  createDirectoryIfMissing True (root </> "assets")
  createDirectoryIfMissing True (root </> "templates")
  
  TIO.writeFile (root </> "sara.yaml") (generateConfig opts)
  TIO.writeFile (root </> "site.hs") generateSiteHs
  TIO.writeFile (root </> "templates" </> "post.html") sampleTemplate
  TIO.writeFile (root </> "posts" </> "hello-world.md") samplePost
  TIO.writeFile (root </> "assets" </> "search.js") searchJs

-- | Scaffolds a project from a user-defined template directory.
scaffoldFromTemplate :: FilePath -> FilePath -> ScaffoldOptions -> IO ()
scaffoldFromTemplate templatePath root opts = do
  putStrLn $ "Scaffolding SARA project from template " ++ templatePath ++ " into " ++ root
  exists <- doesDirectoryExist templatePath
  if not exists
    then putStrLn "Template directory does not exist. Falling back to default scaffold." >> scaffoldProject root opts
    else do
      createDirectoryIfMissing True root
      copyRecursive templatePath root
  where
    copyRecursive src dst = do
      content <- listDirectory src
      forM_ content $ \name -> do
        let srcPath = src </> name
        let dstPath = dst </> name
        isDir <- doesDirectoryExist srcPath
        if isDir
          then createDirectoryIfMissing True dstPath >> copyRecursive srcPath dstPath
          else copyFile srcPath dstPath

generateConfig :: ScaffoldOptions -> Text
generateConfig opts = T.unlines
  [ "title: " <> scTitle opts
  , "author: " <> scAuthor opts
  , "baseUrl: " <> scBaseUrl opts
  , "outputDir: _site"
  , "defaultTemplate: templates/post.html"
  ]

generateSiteHs :: Text
generateSiteHs = T.unlines
  [ "{-# LANGUAGE OverloadedStrings #-}"
  , "import SARA"
  , ""
  , "main :: IO ()"
  , "main = sara $ do"
  , "  -- 1. Infrastructure: Global Metadata Rules"
  , "  remapMetadata [(\"author\", \"SARA User\")]"
  , ""
  , "  -- 2. Assets: Discovery and Optimization"
  , "  discover (glob \"assets/*\")"
  , ""
  , "  -- 3. Content: Validated Pipeline"
  , "  posts <- match (glob \"posts/*.md\") $ \\file -> do"
  , "    item <- readMarkdown file"
  , "    item' <- validateSEO item"
  , "    render \"templates/post.html\" item'"
  , "    pure item'"
  , ""
  , "  -- 4. Features: Inverted Search Index, Sitemap, and RSS"
  , "  case posts of"
  , "    [] -> pure ()"
  , "    ps -> do"
  , "      buildSearchIndex \"search-index.json\" ps"
  , "      buildSitemap \"sitemap.xml\" ps"
  , "      let feedCfg = FeedConfig"
  , "            { feedTitle = \"SARA Feed\""
  , "            , feedDescription = \"Latest updates from SARA\""
  , "            , feedAuthor = \"SARA User\""
  , "            , feedBaseUrl = \"http://localhost:8080\""
  , "            }"
  , "      buildRSS \"feed.xml\" feedCfg ps"
  ]

sampleTemplate :: Text
sampleTemplate = T.unlines
  [ "<!DOCTYPE html>"
  , "<html lang=\"en\">"
  , "<head>"
  , "  <meta charset=\"UTF-8\">"
  , "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
  , "  <meta name=\"view-transition\" content=\"same-origin\">"
  , "  <title>{{ itemTitle }} | {{ siteTitle }}</title>"
  , "  <style>"
  , "    :root { --bg: #ffffff; --fg: #1a1a1a; --accent: #0070f3; --muted: #666; --border: #eee; --modal-bg: rgba(255,255,255,0.9); }"
  , "    @media (prefers-color-scheme: dark) { :root { --bg: #0f0f0f; --fg: #f5f5f5; --accent: #3291ff; --muted: #888; --border: #333; --modal-bg: rgba(15,15,15,0.9); } }"
  , "    body { font-family: system-ui, -apple-system, sans-serif; line-height: 1.6; max-width: 800px; margin: 40px auto; padding: 0 20px; color: var(--fg); background: var(--bg); transition: background 0.3s, color 0.3s; }"
  , "    nav { display: flex; justify-content: space-between; align-items: center; border-bottom: 1px solid var(--border); padding-bottom: 20px; margin-bottom: 40px; }"
  , "    a { color: var(--accent); text-decoration: none; }"
  , "    .search-btn { background: var(--border); border: none; color: var(--muted); padding: 5px 12px; border-radius: 6px; cursor: pointer; font-size: 0.9rem; }"
  , "    #search-modal { display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%; background: var(--modal-bg); backdrop-filter: blur(8px); z-index: 1000; justify-content: center; padding-top: 10vh; }"
  , "    .search-container { width: 100%; max-width: 600px; padding: 20px; }"
  , "    #search-input { width: 100%; padding: 12px; border-radius: 8px; border: 1px solid var(--border); background: var(--bg); color: var(--fg); font-size: 1.2rem; outline: none; box-shadow: 0 4px 12px rgba(0,0,0,0.1); }"
  , "    #search-results { margin-top: 20px; max-height: 60vh; overflow-y: auto; }"
  , "    .search-result { padding: 12px; border-bottom: 1px solid var(--border); cursor: pointer; }"
  , "    .search-result:hover { background: var(--border); }"
  , "    .search-result h3 { margin: 0; font-size: 1.1rem; }"
  , "    .search-result p { margin: 5px 0 0; font-size: 0.9rem; color: var(--muted); }"
  , "    kbd { background: var(--border); padding: 2px 4px; border-radius: 4px; font-size: 0.8rem; }"
  , "    .lqip { background-size: cover; background-repeat: no-repeat; filter: blur(20px); transition: filter 0.5s ease-in-out; }"
  , "    .lqip.loaded { filter: blur(0); }"
  , "  </style>"
  , "</head>"
  , "<body>"
  , "  <nav>"
  , "    <strong>{{ siteTitle }}</strong>"
  , "    <div>"
  , "      <a href=\"/\">Home</a>"
  , "      <button class=\"search-btn\" onclick=\"openSearch()\">Search <kbd>⌘K</kbd></button>"
  , "    </div>"
  , "  </nav>"
  , "  <main>{{{ itemBody }}}</main>"
  , "  <div id=\"search-modal\" onclick=\"closeSearch(event)\">"
  , "    <div class=\"search-container\">"
  , "      <input type=\"text\" id=\"search-input\" placeholder=\"Search anything...\" oninput=\"doSearch(this.value)\" autocomplete=\"off\">"
  , "      <div id=\"search-results\"></div>"
  , "    </div>"
  , "  </div>"
  , "  <script src=\"/assets/search.js\"></script>"
  , "</body>"
  , "</html>"
  ]

samplePost :: Text
samplePost = T.unlines
  [ "---"
  , "title: Hello SARA"
  , "date: 2026-04-16"
  , "---"
  , "Welcome to your new **SARA** site!"
  ]

searchJs :: Text
searchJs = T.unlines
  [ "let searchIndex = null;"
  , ""
  , "async function loadSearchIndex() {"
  , "  if (searchIndex) return;"
  , "  const resp = await fetch('/search-index.json');"
  , "  searchIndex = await resp.json();"
  , "}"
  , ""
  , "function openSearch() {"
  , "  document.getElementById('search-modal').style.display = 'flex';"
  , "  document.getElementById('search-input').focus();"
  , "  loadSearchIndex();"
  , "}"
  , ""
  , "function closeSearch(e) {"
  , "  if (e.target.id === 'search-modal') {"
  , "    document.getElementById('search-modal').style.display = 'none';"
  , "  }"
  , "}"
  , ""
  , "window.addEventListener('keydown', (e) => {"
  , "  if ((e.metaKey || e.ctrlKey) && e.key === 'k') {"
  , "    e.preventDefault();"
  , "    openSearch();"
  , "  }"
  , "  if (e.key === 'Escape') {"
  , "    document.getElementById('search-modal').style.display = 'none';"
  , "  }"
  , "});"
  , ""
  , "function doSearch(query) {"
  , "  if (!searchIndex || query.length < 2) {"
  , "    document.getElementById('search-results').innerHTML = '';"
  , "    return;"
  , "  }"
  , "  const q = query.toLowerCase();"
  , "  const results = [];"
  , "  const docIds = new Set();"
  , "  Object.keys(searchIndex.index).forEach(term => {"
  , "    if (term.includes(q)) { searchIndex.index[term].forEach(id => docIds.add(id)); }"
  , "  });"
  , "  docIds.forEach(id => { results.push(searchIndex.documents[id]); });"
  , "  renderResults(results);"
  , "}"
  , ""
  , "function renderResults(results) {"
  , "  const container = document.getElementById('search-results');"
  , "  container.innerHTML = results.map(res => `"
  , "    <div class=\"search-result\" onclick=\"location.href='${res.seUrl}'\">"
  , "      <h3>${res.seTitle}</h3>"
  , "      <p>${res.seUrl}</p>"
  , "    </div>"
  , "  `).join('');"
  , "}"
  , ""
  , "// --- LIVE PREVIEW LOGIC ---"
  , "const socket = new WebSocket('ws://' + location.host + '/live');"
  , "socket.onopen = () => { socket.send(JSON.stringify({ path: location.pathname })); };"
  , "socket.onmessage = (event) => {"
  , "  const data = JSON.parse(event.data);"
  , "  if (data.type === 'patch' && data.path === location.pathname) {"
  , "    const newHtml = data.html;"
  , "    const parser = new DOMParser();"
  , "    const doc = parser.parseFromString(newHtml, 'text/html');"
  , "    const newMain = doc.querySelector('main').innerHTML;"
  , "    if (document.startViewTransition) {"
  , "      document.startViewTransition(() => {"
  , "        document.querySelector('main').innerHTML = newMain;"
  , "      });"
  , "    } else {"
  , "      document.querySelector('main').innerHTML = newMain;"
  , "    }"
  , "  } else if (data.type === 'reload') {"
  , "    location.reload();"
  , "  }"
  , "};"
  ]
