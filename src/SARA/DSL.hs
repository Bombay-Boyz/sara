{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

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
  , imagePlaceholder
  , regexRoute
  , glob
  , void
  ) where

import SARA.Types
import SARA.Monad (SaraM(..), RuleDecl(..), SaraEnv(..), tellRule)
import SARA.Error (SaraError(..), AnySaraError(..), renderAnyErrorColor)
import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Markdown.Parser (parseMarkdown)
import qualified SARA.Frontmatter.Remap as Remap
import SARA.Asset.Discover (discoverAssets)
import SARA.Markdown.Shortcode (Shortcode(..))
import Development.Shake (liftIO)
import System.FilePath.Glob (globDir1, compile)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified BLAKE3
import qualified Data.Text as T
import Data.Text (Text)
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO as TIO
import qualified Data.Yaml as Yaml
import System.FilePath (takeExtension, replaceExtension)
import qualified SARA.Routing.Engine as REngine

-- | Match source files by glob and run logic for each.
match
  :: GlobPattern
  -> (FilePath -> SaraM (Item 'Validated))
  -> SaraM [Item 'Validated]
match g f = do
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  items <- mapM f files
  tellRule (RuleMatch g f)
  return items

-- | Auto-discover and copy/process assets.
discover :: GlobPattern -> SaraM ()
discover = discoverAssets

-- | Explicitly assign a route to an item.
route :: Route 'Abstract -> Item 'Validated -> SaraM (Item 'Validated)
route r item = do
  res <- liftIO $ REngine.resolveRoute r (itemPath item)
  case res of
    Right resolved -> return $ item { itemRoute = resolved }
    Left err -> throwError [AnySaraError err]

-- | Read a Markdown file into an Item.
readMarkdown :: FilePath -> SaraM (Item 'Unvalidated)
readMarkdown file = readMarkdownWith (\_ -> "") file

-- | Read Markdown with a custom shortcode handler.
readMarkdownWith 
  :: (Shortcode -> Text) 
  -> FilePath 
  -> SaraM (Item 'Unvalidated)
readMarkdownWith customHandler file = do
  env <- ask
  if envIsPlanning env
    then -- During planning, we don't read the file body.
         return $ Item
           { itemPath = file
           , itemRoute = ResolvedRoute (replaceExtension file "html")
           , itemMeta = KM.empty
           , itemBody = ""
           , itemHash = BLAKE3.hash Nothing ([] :: [BS.ByteString])
           }
    else do
      content <- liftIO $ T.decodeUtf8 <$> BS.readFile file
      case parseFrontmatter file content of
        Right (meta, body) -> do
          let rules = envRemapRules env
          case Remap.remapMetadata rules file meta of
            Left err -> throwError [AnySaraError err]
            Right remappedMeta -> do
              -- 2. Expand shortcodes with industrial image support
              let handler sc = case scName sc of
                    "image" -> 
                      let src = Map.findWithDefault "" "src" (scArgs sc)
                          alt = Map.findWithDefault "" "alt" (scArgs sc)
                          -- We use a more robust token format that is less likely to be broken by path characters
                          -- and we'll ensure src doesn't contain the delimiter.
                          safeSrc = T.replace "__" "" src
                          token = "__LQIP__:" <> safeSrc <> "__"
                      in "<picture class=\"lqip\" style=\"background-image: url(" <> token <> ")\"><img src=\"" <> src <> "\" alt=\"" <> alt <> "\" loading=\"lazy\"></picture>"
                    _ -> customHandler sc

              let htmlBody = parseMarkdown handler file body
              return $ Item
                { itemPath = file
                , itemRoute = ResolvedRoute (replaceExtension file "html")
                , itemMeta = remappedMeta
                , itemBody = htmlBody
                , itemHash = BLAKE3.hash Nothing [T.encodeUtf8 content]
                }
        Left err -> throwError [AnySaraError err]

-- | Validate SEO properties.
validateSEO :: Item 'Unvalidated -> SaraM (Item 'Validated)
validateSEO item = do
  env <- ask
  if envIsPlanning env
    then -- Coerce type for planning phase
      return $ Item
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
        (Nothing, _) -> throwError [AnySaraError $ SEOTitleMissing (itemPath item)]
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
render :: FilePath -> Item 'Validated -> SaraM ()
render tpl item = do
  let outPath = case itemRoute item of
                  ResolvedRoute p -> p
  tellRule (RuleRender tpl item outPath)

-- | Render an Item using a custom Haskell-based renderer.
renderWith :: (Item 'Validated -> Text) -> Item 'Validated -> SaraM ()
renderWith renderer item = do
  let outPath = case itemRoute item of
                  ResolvedRoute p -> p
  tellRule (RuleRenderRaw (renderer item) item outPath)

-- | Register metadata remapping rules.
remapMetadata :: [(Text, Text)] -> SaraM ()
remapMetadata rules = tellRule (RuleRemap rules)

-- | Register a search index generation rule.
buildSearchIndex :: FilePath -> [Item 'Validated] -> SaraM ()
buildSearchIndex outPath ps = tellRule (RuleSearch outPath ps)

-- | Register a sitemap.xml generation rule.
buildSitemap :: FilePath -> [Item 'Validated] -> SaraM ()
buildSitemap outPath ps = tellRule (RuleSitemap outPath ps)

-- | Register an RSS feed generation rule.
buildRSS :: FilePath -> FeedConfig -> [Item 'Validated] -> SaraM ()
buildRSS outPath cfg ps = tellRule (RuleRSS outPath cfg ps)

-- | Loads structured data (JSON or YAML) from a file.
--   Automatically tracks dependencies in Shake.
loadData :: FilePath -> SaraM Aeson.Value
loadData path = do
  -- 1. Tell Shake to track this file as a dependency.
  tellRule (RuleDataDependency path)
  
  -- 2. Read the file NOW (planning) to allow the DSL to use the data.
  content <- liftIO $ BS.readFile path
  let ext = takeExtension path
  case ext of
    ".json" -> case Aeson.decodeStrict content of
                 Just v -> return v
                 Nothing -> throwError [AnySaraError $ AssetProcessingFailed path "Failed to parse JSON"]
    ".yaml" -> case Yaml.decodeEither' content of
                 Right v -> return v
                 Left err -> throwError [AnySaraError $ AssetProcessingFailed path (T.pack $ show err)]
    _       -> throwError [AnySaraError $ AssetProcessingFailed path (T.pack $ "Unsupported data format: " ++ ext)]

-- | Generates a Base64 LQIP magic token for an image.
imagePlaceholder :: FilePath -> SaraM Text
imagePlaceholder path = return $ "__LQIP__:" <> T.pack path <> "__"

-- | Smart constructor for regex routes.
regexRoute :: Text -> Text -> SaraM (Route 'Abstract)
regexRoute pat repl = do
  res <- liftIO $ REngine.regexRoute pat repl
  case res of
    Right r -> return r
    Left err -> throwError [AnySaraError err]

-- | Convenience helper for glob patterns.
glob :: Text -> GlobPattern
glob = GlobPattern

void :: Functor f => f a -> f ()
void = fmap (const ())
