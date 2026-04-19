{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module SARA.DSL
  ( match
  , discover
  , route
  , readMarkdown
  , readMarkdownWith
  , validateSEO
  , render
  , renderWith
  , remapMetadata
  , buildSearchIndex
  , buildSitemap
  , buildRSS
  , loadData
  , paginate
  , renderPager
  , renderPagerWith
  , registerTaxonomy
  , getTaxonomy
  , registerShortcode
  , imagePlaceholder
  , regexRoute
  , glob
  , void
  , SPath
  , chunk
  ) where

import SARA.Types
import SARA.Monad (SaraM(..), SaraEnv(..), SaraState(..), RuleDecl(..), tellRule, commitRules, readFileTracked, readTextFileTracked)
import SARA.Error (SaraError(..), AnySaraError(..), renderAnyErrorColor)
import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Markdown.Parser (parseMarkdown)
import qualified SARA.Frontmatter.Remap as Remap
import SARA.Asset.Discover (discoverAssets)
import SARA.Markdown.Shortcode (Shortcode(..))
import Development.Shake (liftIO)
import System.FilePath.Glob (globDir1, compile)
import UnliftIO.IORef (atomicModifyIORef', readIORef)
import Control.Monad (void, forM_)
import Control.Monad.Reader (ask)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import qualified BLAKE3
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.FilePath (takeExtension)
import qualified SARA.Routing.Engine as REngine
import UnliftIO.Exception (throwIO)

-- | Match source files by glob and run logic for each.
--   Industrial Grade: Enforces 'Validated constraint at the type level.
--   User must call 'validateSEO' or equivalent to enter the build graph.
match
  :: GlobPattern
  -> (SPath -> SaraM (Item 'Validated))
  -> SaraM [Item 'Validated]
match g f = do
  env <- ask
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ map T.pack <$> globDir1 (compile patStr) "."
  items <- mapM f files
  
  -- Industrial fix: In planning mode, we must record these items in the cache
  -- so collectOutputs can find their routed paths for the 'want' list.
  if envIsPlanning env
    then do
      forM_ items $ \i -> do
        liftIO $ atomicModifyIORef' (envState env) $ \s ->
          (s { stateItemCache = Map.insert (itemPath i) i (stateItemCache s) }, ())
    else return ()

  tellRule (RuleMatch g f)
  commitRules
  return items

-- | Auto-discover and copy/process assets.
discover :: GlobPattern -> SaraM ()
discover g = discoverAssets g >> commitRules

-- | Explicitly assign a route to an item.
route :: Route 'Abstract -> Item 'Validated -> SaraM (Item 'Validated)
route r item = do
  res <- liftIO $ REngine.resolveRoute r (T.unpack $ itemPath item)
  case res of
    Right resolved -> return $ item { itemRoute = resolved }
    Left err -> throwIO (AnySaraError err)

-- | Read a Markdown file into an Item.
readMarkdown :: SPath -> SaraM (Item 'Planning)
readMarkdown file = readMarkdownWith (\_ -> return "") file

-- | Read Markdown with a custom shortcode handler.
readMarkdownWith :: (Shortcode -> SaraM Text) -> SPath -> SaraM (Item 'Planning)
readMarkdownWith customHandler file = do
  env <- ask
  content <- readTextFileTracked file
  case parseFrontmatter file content of
    Right (meta, body) -> do
      let rules = envRemapRules env
      let (warnings, remappedMeta) = Remap.remapMetadata rules (T.unpack file) meta
      -- Emit warnings but don't fail
      forM_ warnings (\w -> liftIO $ TIO.putStrLn (renderAnyErrorColor (AnySaraError w)))
      
      -- Industrial fix: We always parse the body to ensure shortcodes are expanded
      -- and metadata is fully available even during planning.
      state <- readIORef (envState env)
      let registry = stateShortcodeHandlers state
          -- 2. Expand shortcodes with industrial image support + registry
          handler sc = case scName sc of
                "image" -> 
                  let src = Map.findWithDefault "" "src" (scArgs sc)
                      alt = Map.findWithDefault "" "alt" (scArgs sc)
                      safeSrc = T.replace "__" "" src
                      token = "{{SARA_LQIP:" <> safeSrc <> "}}"
                  in return $ "<picture class=\"lqip\" style=\"background-image: url(" <> token <> ")\"><img src=\"" <> src <> "\" alt=\"" <> alt <> "\" loading=\"lazy\"></picture>"
                name -> case Map.lookup name registry of
                  Just h  -> h sc
                  Nothing -> customHandler sc

      htmlBody <- parseMarkdown handler (T.unpack file) body
      
      return $ Item
        { itemPath = file
        , itemRoute = UnresolvedRoute
        , itemMeta = remappedMeta
        , itemBody = htmlBody
        , itemHash = BLAKE3.hash Nothing [T.encodeUtf8 content]
        }
    Left err -> throwIO (AnySaraError err)

-- | Validate SEO properties.
validateSEO :: Item 'Planning -> SaraM (Item 'Validated)
validateSEO item = do
  env <- ask
  if envIsPlanning env
    then return $ Item
           { itemPath = itemPath item
           , itemRoute = itemRoute item
           , itemMeta = itemMeta item
           , itemBody = itemBody item
           , itemHash = itemHash item
           }
    else do
      let meta = itemMeta item
      let title = KM.lookup (K.fromText "title") meta
      let desc = KM.lookup (K.fromText "description") meta
      case (title, desc) of
        (Just _, Just _) -> return $ Item
          { itemPath = itemPath item
          , itemRoute = itemRoute item
          , itemMeta = itemMeta item
          , itemBody = itemBody item
          , itemHash = itemHash item
          }
        (Nothing, _) -> throwIO (AnySaraError $ SEOTitleMissing (itemPath item))
        (_, Nothing) -> do
          -- Return as a warning instead of a hard error
          liftIO $ TIO.putStrLn $ renderAnyErrorColor $ AnySaraError $ SEODescriptionMissing (itemPath item)
          return $ Item
            { itemPath = itemPath item
            , itemRoute = itemRoute item
            , itemMeta = itemMeta item
            , itemBody = itemBody item
            , itemHash = itemHash item
            }

-- | Render an Item through a template.
render :: SPath -> Item 'Validated -> SaraM ()
render tpl item = do
  -- If item has UnresolvedRoute, apply default SlugRoute
  resolvedItem <- case itemRoute item of
    UnresolvedRoute -> do
      res <- liftIO $ REngine.resolveRoute SlugRoute (T.unpack $ itemPath item)
      case res of
        Right r -> return $ item { itemRoute = r }
        Left err -> throwIO (AnySaraError err)
    _ -> return item
  let outPath = case itemRoute resolvedItem of
                  ResolvedRoute p -> p
                  _               -> error "impossible: route should be resolved here"
  tellRule (RuleRender tpl resolvedItem outPath)
  commitRules

-- | Render an Item using a custom Haskell-based renderer.
renderWith :: (Item 'Validated -> Text) -> Item 'Validated -> SaraM ()
renderWith renderer item = do
  -- Similar deferred resolution for renderWith
  resolvedItem <- case itemRoute item of
    UnresolvedRoute -> do
      res <- liftIO $ REngine.resolveRoute SlugRoute (T.unpack $ itemPath item)
      case res of
        Right r -> return $ item { itemRoute = r }
        Left err -> throwIO (AnySaraError err)
    _ -> return item
  let outPath = case itemRoute resolvedItem of
                  ResolvedRoute p -> p
                  _               -> error "impossible"
  tellRule (RuleRenderRaw (renderer resolvedItem) resolvedItem outPath)
  commitRules

-- | Slices a list of items and allows running logic for each page.
--   Industrial Fix for Hugo/Hakyll: 
--   Provides full control over path generation (\n -> "blog/" ++ show n)
--   while maintaining type-safety and Shake dependencies.
paginate 
  :: Int                               -- ^ Items per page
  -> [Item 'Validated]                 -- ^ Items to paginate
  -> (Int -> SPath)                    -- ^ Path generator (page number to output path)
  -> (Pager 'Validated -> SPath -> SaraM ()) -- ^ Action (e.g. \p path -> renderPager "tpl.html" p path)
  -> SaraM ()
paginate size items pathGen f = do
  let totalItems = length items
      totalPages = (totalItems + size - 1) `div` size
      chunks     = chunk size items
      pages      = zip [1..] chunks

  forM_ pages $ \(n, chunkItems) -> do
    let pagePath = pathGen n
        pager = Pager
          { pagerItems    = chunkItems
          , pagerCurrent  = n
          , pagerTotal    = totalPages
          , pagerPageSize = size
          , pagerHasNext  = n < totalPages
          , pagerHasPrev  = n > 1
          , pagerNextUrl  = if n < totalPages then Just (pathGen (n + 1)) else Nothing
          , pagerPrevUrl  = if n > 1          then Just (pathGen (n - 1)) else Nothing
          }
    f pager pagePath

-- | Render a Pager through a template.
renderPager :: SPath -> Pager 'Validated -> SPath -> SaraM ()
renderPager tpl pager outPath = tellRule (RuleRenderPager tpl pager outPath) >> commitRules

-- | Render a Pager with a custom Haskell-based renderer.
renderPagerWith :: (Pager 'Validated -> Text) -> Pager 'Validated -> SPath -> SaraM ()
renderPagerWith renderer pager outPath = do
  -- Industrial fix: Use RuleRenderRaw for pagers by wrapping it
  let html = renderer pager
  -- Create a dummy Item to satisfy RuleRenderRaw
  let dummy = Item 
        { itemPath = outPath
        , itemRoute = ResolvedRoute outPath
        , itemMeta = KM.empty
        , itemBody = html
        , itemHash = BLAKE3.hash Nothing [T.encodeUtf8 html]
        }
  tellRule (RuleRenderRaw html dummy outPath)
  commitRules

-- Helper: Split a list into chunks of fixed size
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs = let (h, t) = splitAt n xs in h : chunk n t

-- | Register metadata remapping rules.
remapMetadata :: [(Text, Text)] -> SaraM ()
remapMetadata rules = tellRule (RuleRemap rules) >> commitRules

-- | Register a search index generation rule.
buildSearchIndex :: SPath -> [Item 'Validated] -> SaraM ()
buildSearchIndex outPath ps = tellRule (RuleSearch outPath ps) >> commitRules

-- | Register a sitemap.xml generation rule.
buildSitemap :: SPath -> [Item 'Validated] -> SaraM ()
buildSitemap outPath ps = tellRule (RuleSitemap outPath ps) >> commitRules

-- | Register an RSS feed generation rule.
buildRSS :: SPath -> FeedConfig -> [Item 'Validated] -> SaraM ()
buildRSS outPath cfg ps = tellRule (RuleRSS outPath cfg ps) >> commitRules

-- | Register a taxonomy (e.g., "tags") and group items by it.
registerTaxonomy :: Text -> [Item v] -> SaraM ()
registerTaxonomy key items = do
  env <- ask
  let taxMap = L.foldl' groupItem Map.empty items
  let tax = Taxonomy key taxMap
  atomicModifyIORef' (envState env) $ \s ->
    (s { stateTaxonomies = Map.insert key tax (stateTaxonomies s) }, ())
  where
    groupItem acc item =
      let meta = itemMeta item
          vals = case KM.lookup (K.fromText key) meta of
                   Just (Aeson.String v) -> [v]
                   Just (Aeson.Array arr) -> [ v | Aeson.String v <- V.toList arr ]
                   _ -> []
      in L.foldl' (\a v -> Map.insertWith (++) v [itemPath item] a) acc vals

-- | Retrieve a registered taxonomy.
getTaxonomy :: Text -> SaraM (Maybe Taxonomy)
getTaxonomy key = do
  env <- ask
  state <- readIORef (envState env)
  return $ Map.lookup key (stateTaxonomies state)

-- | Loads structured data (JSON or YAML) from a file.
--   Automatically tracks dependencies in Shake and caches the result for the build session.
loadData :: SPath -> SaraM Aeson.Value
loadData path = do
  env <- ask
  -- 1. Check cache first
  state <- liftIO $ readIORef (envState env)
  case Map.lookup path (stateDataCache state) of
    Just v -> return v
    Nothing -> do
      -- 2. Read the file with automatic tracking.
      content <- readFileTracked path

      let ext = takeExtension (T.unpack path)
      val <- case ext of
        ".json" -> case Aeson.decodeStrict content of
                     Just v -> return v
                     Nothing -> throwIO (AnySaraError $ AssetProcessingFailed (T.unpack path) "Failed to parse JSON")
        ".yaml" -> case Yaml.decodeEither' content of
                     Right v -> return v
                     Left err -> throwIO (AnySaraError $ AssetProcessingFailed (T.unpack path) (T.pack $ show err))
        _       -> throwIO (AnySaraError $ AssetProcessingFailed (T.unpack path) (T.pack $ "Unsupported data format: " ++ ext))

      -- 3. Update cache
      liftIO $ atomicModifyIORef' (envState env) $ \s ->
        (s { stateDataCache = Map.insert path val (stateDataCache s) }, ())
      return val
-- | Register a custom shortcode handler.
registerShortcode :: Text -> (Shortcode -> SaraM Text) -> SaraM ()
registerShortcode name handler = do
  env <- ask
  atomicModifyIORef' (envState env) $ \s ->
    (s { stateShortcodeHandlers = Map.insert name handler (stateShortcodeHandlers s) }, ())

-- | Generates a Base64 LQIP magic token for an image.
imagePlaceholder :: SPath -> SaraM Text
imagePlaceholder path = return $ "{{SARA_LQIP:" <> path <> "}}"

-- | Smart constructor for regex routes.
regexRoute :: Text -> Text -> SaraM (Route 'Abstract)
regexRoute pat repl = do
  res <- liftIO $ REngine.regexRoute pat repl
  case res of
    Right r -> return r
    Left err -> throwIO (AnySaraError err)

-- | Convenience helper for glob patterns.
glob :: Text -> GlobPattern
glob = GlobPattern
