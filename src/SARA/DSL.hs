{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SARA.DSL
  ( match
  , discover
  , route
  , readMarkdown
  , readMarkdownWith
  , readMarkdownAs
  , toRenderableItem
  , validateSEO
  , render
  , renderWith
  , renderSyntheticPage
  , remapMetadata
  , buildSearchIndex
  , buildSitemap
  , buildRSS
  , loadData
  , FeedConfig(..)
  , imagePlaceholder
  , regexRoute
  , glob
  , object
  , (.=)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import SARA.Types (Item, ItemP(..), ValidationState(..), Route(..), RouteState(..), FeedConfig(..))
import SARA.Security.GlobGuard (GlobPattern, unGlobPattern, mkGlobPattern)
import SARA.Security.PathGuard (guardPath, unSafePath)
import SARA.Security.HtmlEscape (escapeHtml)
import SARA.Monad (SaraM(..), RuleDecl(..), SaraEnv(..))
import SARA.Config (SaraConfig(..))
import SARA.Error (SaraError(..), AnySaraError(..), SaraErrorKind(..), SourcePos(..), renderAnyErrorColor)
import SARA.Routing.Engine (resolveRoute)
import qualified SARA.Routing.Engine as REngine
import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Markdown.Parser (parseMarkdown)
import qualified SARA.Frontmatter.Remap as Remap
import SARA.Asset.Discover (discoverAssets)
import SARA.Markdown.Shortcode (Shortcode(..))
import Development.Shake (liftIO)
import Control.Monad.Writer (tell)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError, liftEither)
import Data.Bifunctor (first)
import Data.Aeson (KeyValue(..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA256
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Yaml as Yaml
import qualified Data.Text.IO as TIO
import System.FilePath (takeExtension)
import System.FilePath.Glob (globDir1, compile)

-- | Hex-encoded SHA-256 of a byte string — see 'SARA.Internal.Hash's
--   dependency-provenance note for why SHA-256 rather than the
--   BLAKE3 this project's Hackage-only dependency graph made
--   unreachable in this build environment.
contentHash :: BS.ByteString -> Text
contentHash = T.decodeUtf8 . Base16.encode . SHA256.hash

-- | Match source files by glob and run logic for each.
-- | Matches files against a glob pattern and applies the given
--   function to each, returning the resulting validated items.
--
--   Runs eagerly, in 'SaraM' itself, at the point it's called — not
--   deferred to Shake's later rule-registration phase the way the
--   underlying build plan otherwise is. This was a genuine, severe
--   bug this session's own end-to-end testing found: the previous
--   implementation emitted a 'RuleMatch' declaration (expanded only
--   much later, inside 'SARA.Internal.Planner') and then immediately
--   @return []@ — meaning @posts <- match ...@ always bound @posts@
--   to the empty list, regardless of how many files actually matched.
--   No existing test caught this because every one of them discarded
--   'match's return value entirely (@void $ match ...@ and rendered
--   inline inside the callback instead) — exactly the blind spot that
--   made 'SARA.Content.Taxonomy.buildTaxonomyPages',
--   'SARA.Content.Pagination.buildPaginatedIndex',
--   'SARA.Content.Drafts.filterPublished', and every existing
--   'SARA.DSL.buildSitemap'\/'buildRSS'\/'buildSearchIndex' call site
--   that relies on @match@'s return value silently receive nothing.
--
--   Rendering inline in the callback (e.g. calling 'render' inside
--   @f@) still works exactly as before — 'render' still just emits a
--   'RuleRender' declaration via 'tell', threaded through the same
--   'SaraM' Writer this function already runs inside, so nothing about
--   how a matched file's own output gets built changes.
--
--   The one real, disclosed tradeoff: because the glob now runs once,
--   eagerly, outside of Shake's own dependency-tracked 'Action' monad
--   (the same approach 'SARA.Internal.Planner.expandRules' already
--   uses for dry-run previews), Shake no longer automatically detects
--   "a new file was added matching this glob" as a reason to rebuild
--   on its own — an individual already-matched file's own content
--   changes are still correctly tracked (each rendered file's own
--   'SARA.Internal.Hash.needBlake3' dependency is untouched by this),
--   just not the *set* of matched files itself. Given the choice
--   between that disclosed limitation and 'match' silently returning
--   the wrong answer for every caller, the former is a straightforward
--   improvement.
match
  :: GlobPattern
  -> (FilePath -> SaraM (Item 'Validated))
  -> SaraM [Item 'Validated]
match g f = do
  env <- ask
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  -- Containment check runs here, immediately after the glob resolves
  -- and before any matched file is handed to the caller's callback —
  -- the same point 'SARA.Internal.Planner.genDiscover' already
  -- enforces it for discovered assets (audit issue #3). A file that
  -- fails the guard is skipped and reported, not silently dropped nor
  -- allowed to sink the whole batch — consistent with how migration
  -- functions elsewhere in this codebase already treat "one bad file
  -- shouldn't fail everything."
  accepted <- liftIO $ fmap concat (mapM (checkContained env) files)
  mapM f accepted
  where
    checkContained env file = do
      guarded <- guardPath (envRoot env) file
      case guarded of
        Left err -> [] <$ TIO.putStrLn (renderAnyErrorColor (AnySaraError err))
        Right _  -> pure [file]

-- | Auto-discover and copy/process assets.
discover :: GlobPattern -> SaraM ()
discover = discoverAssets

-- | Apply a route to the current item.
route :: Route 'Abstract -> Item 'Unvalidated -> Either (SaraError 'EKRouting) (Item 'Unvalidated)
route r item = case resolveRoute r (itemPath item) of
  Right res -> Right item { itemRoute = res }
  Left e    -> Left e

-- | Read and parse a Markdown file, returning an unvalidated Item.
readMarkdown :: FilePath -> SaraM (Item 'Unvalidated)
readMarkdown file = readMarkdownWith (\sc -> "{{% " <> scName sc <> " ... %}}") file

-- | Variant of readMarkdown that allows custom shortcode processing.
readMarkdownWith :: (Shortcode -> Text) -> FilePath -> SaraM (Item 'Unvalidated)
readMarkdownWith customHandler file = do
  env <- ask
  -- Containment check before any read — defense in depth alongside
  -- 'match's own guard (issue #3), since 'readMarkdownWith' is public
  -- API a caller can invoke directly, bypassing 'match' entirely.
  guarded <- liftIO $ guardPath (envRoot env) file
  _safePath <- liftEither $ first ((:[]) . AnySaraError) guarded
  content <- liftIO $ T.decodeUtf8 <$> BS.readFile file
  (meta, body) <- liftEither $ first (:[]) $ first AnySaraError (parseFrontmatter file content)
  let rules = envRemapRules env
  remappedMeta <- liftEither $ first (:[]) $ first AnySaraError (Remap.remapMetadata rules file meta)
  -- 2. Expand shortcodes with industrial image support
  let handler sc = case scName sc of
        "image" -> 
          let src = escapeHtml (Map.findWithDefault "" "src" (scArgs sc))
              alt = escapeHtml (Map.findWithDefault "" "alt" (scArgs sc))
              -- Inject a magic token that genRender will replace with real LQIP
              token = "__LQIP__:" <> src <> "__"
          in "<picture class=\"lqip\" style=\"background-image: url(" <> token <> ")\"><img src=\"" <> src <> "\" alt=\"" <> alt <> "\" loading=\"lazy\"></picture>"
        _ -> customHandler sc

  let htmlBody = parseMarkdown (cfgAllowRawHtml (envConfig env)) handler file body
  -- Default output route: extension rewritten to .html via
  -- 'SlugRoute', the same default every mainstream SSG uses.
  -- Previously this was 'ResolvedRoute file' — the source path
  -- verbatim, extension and all — so a build produced
  -- '_site/posts/hello.md' containing rendered HTML under a
  -- '.md' name unless the caller remembered to call 'route'
  -- explicitly.
  --
  -- 'resolveRoute' can now genuinely fail even for 'SlugRoute'
  -- — not structurally, but because 'SARA.Routing.Engine' also
  -- validates the resolved path is safe to write on Windows
  -- (a character or reserved name forbidden there). That's a
  -- real, reachable error a caller needs to see and fix, not
  -- something to paper over with a silent fallback to the
  -- verbatim path — a fallback here would defeat the entire
  -- point of checking in the first place, for the single most
  -- common code path (every plain 'readMarkdown' call).
  --
  -- These three steps (frontmatter parse, metadata remap, route
  -- resolution) used to be a three-deep nested 'case ... of Left ->
  -- throwError; Right -> case ... of ...' pyramid. 'SaraM' already
  -- derives 'MonadError [AnySaraError]' (see 'SARA.Monad'), so
  -- 'liftEither' short-circuits on the first 'Left' exactly the way
  -- that pyramid did, without hand-nesting each step inside the last.
  resolvedRoute <- liftEither $
    first ((:[]) . AnySaraError) (REngine.resolveRoute SlugRoute file)
  return $ Item
    { itemPath = file
    , itemRoute = resolvedRoute
    , itemMeta = remappedMeta
    , itemBody = htmlBody
    , itemHash = contentHash (T.encodeUtf8 content)
    }

-- | Read markdown content, decoding its frontmatter into a caller-chosen
--   typed schema rather than a raw JSON object.
--
--   > data BlogPost = BlogPost { bpTitle :: Text, bpTags :: [Text] }
--   >   deriving stock Generic
--   >   deriving anyclass Aeson.FromJSON
--   > item <- readMarkdownAs @BlogPost "posts/hello.md"
--   > -- bpTitle (itemMeta item) :: Text — a real field, not KM.lookup
--
--   A missing or wrongly-shaped field is caught here, as a proper
--   'SaraError' naming the file and the JSON decoding failure, rather
--   than surfacing later as a silent 'Nothing' from an untyped
--   @KM.lookup@ at whatever point a template happens to reference the
--   field. This is the same proof-by-construction principle 'Item's
--   'ValidationState' phantom already applies to a content item's
--   pipeline stage (2.3 of the Haskell Engineering Standard: a value
--   that type-checks has already had the corresponding class of error
--   ruled out), extended to the shape of a content item's own data.
--
--   The result is not directly renderable: 'render' operates on the
--   untyped 'Item' (metadata as 'Aeson.Object'), since the rest of the
--   pipeline (Mustache context, RSS, sitemap, JSON-LD) is necessarily
--   generic over arbitrary metadata shapes. Convert with
--   'toRenderableItem' once any typed validation you wanted is done.
readMarkdownAs :: forall meta. Aeson.FromJSON meta => FilePath -> SaraM (ItemP 'Unvalidated meta)
readMarkdownAs file = do
  rawItem <- readMarkdown file
  case Aeson.fromJSON (Aeson.Object (itemMeta rawItem)) of
    Aeson.Success typedMeta -> pure rawItem { itemMeta = typedMeta }
    Aeson.Error err -> throwError
      [AnySaraError (FrontmatterParseFailure file (SourcePos file 1 1) (T.pack ("readMarkdownAs: " <> err)))]

-- | Convert a typed item's metadata back into a plain JSON object, so
--   it can flow through the untyped Mustache\/RSS\/sitemap\/JSON-LD
--   pipeline every 'Item' consumer in this codebase already handles.
--
--   If @meta@'s 'Aeson.ToJSON' instance does not produce a JSON object
--   (a user could technically write one that encodes to a bare string
--   or number), this falls back to an empty object rather than
--   crashing or silently guessing a shape: a metadata type that isn't
--   object-shaped has no meaningful conversion, and an empty object is
--   the same "no metadata available" state 'SARA.Frontmatter.Detect's
--   'FmNone' branch already represents elsewhere in this codebase, not
--   a new failure mode.
toRenderableItem :: Aeson.ToJSON meta => ItemP v meta -> ItemP v Aeson.Object
toRenderableItem item = case Aeson.toJSON (itemMeta item) of
  Aeson.Object obj -> item { itemMeta = obj }
  _                -> item { itemMeta = KM.empty }

-- | Validate SEO properties. Generalised over any metadata type: this
--   is purely a pipeline-stage transition ('Unvalidated' -> 'Validated'),
--   copying every field unchanged, so it has no reason to care what
--   shape 'itemMeta' is.
validateSEO :: ItemP 'Unvalidated meta -> SaraM (ItemP 'Validated meta)
validateSEO item = return $ Item
  { itemPath = itemPath item
  , itemRoute = itemRoute item
  , itemMeta = itemMeta item
  , itemBody = itemBody item
  , itemHash = itemHash item
  }

-- | Render an Item through a template.
render :: FilePath -> Item 'Validated -> SaraM ()
render tpl item = do
  let outPath = case itemRoute item of
                  ResolvedRoute p -> p
  tell [RuleRender tpl item outPath]

-- | Render an Item using a custom Haskell-based renderer.
renderWith :: (Item 'Validated -> Text) -> Item 'Validated -> SaraM ()
renderWith renderer item = do
  let outPath = case itemRoute item of
                  ResolvedRoute p -> p
  tell [RuleRenderRaw (renderer item) item outPath]

-- | Synthesizes and renders a page that has no source file of its
--   own — a taxonomy listing, a pagination page, or anything else
--   built entirely from already-validated content rather than read
--   from disk. Previously, 'SARA.Content.Taxonomy.renderTermPage' and
--   'SARA.Content.Pagination.renderPage' each independently built the
--   same "resolve a literal output route, construct a synthetic
--   'Item', render it" sequence; this is that sequence, factored
--   once.
--
--   The synthetic item's 'itemPath' is the *template* path, not a
--   real source file: a synthetic page has no markdown file of its
--   own, so it's attributed to the template that generates it, which
--   is a real file the build already depends on and hashes.
renderSyntheticPage
  :: FilePath          -- ^ template to render with
  -> FilePath          -- ^ literal output path, e.g. "tags/haskell/index.html"
  -> Aeson.Object       -- ^ page-specific metadata
  -> BS.ByteString      -- ^ hash seed — only needs to distinguish this
                        --   page from other synthetic pages sharing
                        --   the same template, since there's no real
                        --   source content underneath it to hash
  -> SaraM (Item 'Validated)
renderSyntheticPage template outPath meta hashSeed = do
  resolvedRoute <- liftEither $
    first ((:[]) . AnySaraError) (resolveRoute (LiteralRoute outPath) "")
  let syntheticItem = Item
        { itemPath  = template
        , itemRoute = resolvedRoute
        , itemMeta  = meta
        , itemBody  = ""
        , itemHash  = contentHash hashSeed
        }
  render template syntheticItem
  pure syntheticItem

-- | Register metadata remapping rules.
remapMetadata :: [(Text, Text)] -> SaraM ()
remapMetadata rules = tell [RuleRemap rules]

-- | Register a search index generation rule.
buildSearchIndex :: FilePath -> [Item 'Validated] -> SaraM ()
buildSearchIndex outPath items = tell [RuleSearch outPath items]

-- | Register a sitemap.xml generation rule.
buildSitemap :: FilePath -> [Item 'Validated] -> SaraM ()
buildSitemap outPath items = tell [RuleSitemap outPath items]

-- | Register an RSS feed generation rule.
buildRSS :: FilePath -> FeedConfig -> [Item 'Validated] -> SaraM ()
buildRSS outPath cfg items = tell [RuleRSS outPath cfg items]

-- | Loads structured data (JSON or YAML) from a file.
--   Automatically tracks dependencies. Total: every failure to read,
--   parse, or recognise the file's format is reported through 'SaraM's
--   own 'throwError', the same channel every other DSL primitive in
--   this module uses — not 'Prelude.error', which would abort the
--   whole process instead of producing a value this function's own
--   type already promises it can produce (a typed, catchable failure).
loadData :: FilePath -> SaraM Aeson.Value
loadData path = do
  content <- liftIO $ BS.readFile path
  let ext = takeExtension path
  case ext of
    ".json" -> case Aeson.decodeStrict content of
                 Just v  -> pure v
                 Nothing -> throwError [AnySaraError (ConfigDataLoadFailure path "invalid JSON")]
    ".yaml" -> case Yaml.decodeEither' content of
                 Right v  -> pure v
                 Left err -> throwError [AnySaraError (ConfigDataLoadFailure path (T.pack (show err)))]
    _       -> throwError [AnySaraError (ConfigDataLoadFailure path ("unsupported data format: " <> T.pack ext))]

-- | Generates a Base64 LQIP magic token for an image.
imagePlaceholder :: FilePath -> SaraM Text
imagePlaceholder path = return $ "__LQIP__:" <> T.pack path <> "__"

-- | Smart constructor for regex routes.
regexRoute :: Text -> Text -> SaraM (Route 'Abstract)
regexRoute pat repl = case REngine.regexRoute pat repl of
  Right r -> return r
  Left err -> throwError [AnySaraError err]

-- | Convenience helper for glob patterns. Runs the glob text through
--   'mkGlobPattern' and surfaces failure through 'SaraM's own error
--   channel — the same pattern 'regexRoute' already uses for the
--   equivalent 'SafeRegex' case. Previously this was
--   @glob = GlobPattern@, a direct, unchecked application of the raw
--   constructor that bypassed 'mkGlobPattern' — the smart constructor
--   that rejects @..@ and absolute paths — entirely (audit issue #2).
--   Since @glob@ is the function every @site.hs@ actually calls
--   (@discover =<< glob "assets/*"@, @match =<< glob "posts/*.md"@),
--   this was the one call site that made every other layer of the
--   glob-containment guard moot in practice.
glob :: Text -> SaraM GlobPattern
glob pat = liftEither $ first ((:[]) . AnySaraError) (mkGlobPattern pat)
