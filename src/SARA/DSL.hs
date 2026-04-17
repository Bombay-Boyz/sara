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
import SARA.Types (GlobPattern(..), Item(..), ValidationState(..), Route(..), RouteState(..), FeedConfig(..))
import SARA.Monad (SaraM(..), RuleDecl(..), SaraEnv(..))
import SARA.Error (SaraError(..), AnySaraError(..), SaraErrorKind(..))
import SARA.Routing.Engine (resolveRoute)
import qualified SARA.Routing.Error as RE
import qualified SARA.Routing.Engine as REngine
import SARA.Frontmatter.Parser (parseFrontmatter)
import SARA.Markdown.Parser (parseMarkdown)
import qualified SARA.Frontmatter.Remap as Remap
import SARA.Asset.Discover (discoverAssets)
import SARA.Markdown.Shortcode (Shortcode(..))
import Development.Shake (liftIO)
import Control.Monad.Writer (tell)
import Control.Monad.Reader (ask)
import Control.Monad.Except (throwError)
import Data.Aeson (KeyValue(..), object)
import qualified Data.Aeson as Aeson
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified BLAKE3
import qualified Data.Yaml as Yaml
import System.FilePath (takeExtension)

-- | Match source files by glob and run logic for each.
match
  :: GlobPattern
  -> (FilePath -> SaraM (Item 'Validated))
  -> SaraM [Item 'Validated]
match g f = do
  tell [RuleMatch g (void . f)]
  return [] 

-- | Auto-discover and copy/process assets.
discover :: GlobPattern -> SaraM ()
discover = discoverAssets

-- | Apply a route to the current item.
route :: Route 'Abstract -> Item 'Unvalidated -> Either (SaraError 'EKRouting) (Item 'Unvalidated)
route r item = case resolveRoute r (itemPath item) of
  Right res -> Right item { itemRoute = res }
  Left (RE.RouteRegexInvalid p d) -> Left $ RouteRegexInvalid p d
  Left (RE.RouteConflict f1 f2 o) -> Left $ RouteConflict f1 f2 o

-- | Read and parse a Markdown file, returning an unvalidated Item.
readMarkdown :: FilePath -> SaraM (Item 'Unvalidated)
readMarkdown file = readMarkdownWith (\sc -> "{{% " <> scName sc <> " ... %}}") file

-- | Variant of readMarkdown that allows custom shortcode processing.
readMarkdownWith :: (Shortcode -> Text) -> FilePath -> SaraM (Item 'Unvalidated)
readMarkdownWith customHandler file = do
  env <- ask
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
                      -- Inject a magic token that genRender will replace with real LQIP
                      token = "__LQIP__:" <> src <> "__"
                  in "<picture class=\"lqip\" style=\"background-image: url(" <> token <> ")\"><img src=\"" <> src <> "\" alt=\"" <> alt <> "\" loading=\"lazy\"></picture>"
                _ -> customHandler sc

          let htmlBody = parseMarkdown handler file body
          return $ Item
            { itemPath = file
            , itemRoute = ResolvedRoute file
            , itemMeta = remappedMeta
            , itemBody = htmlBody
            , itemHash = BLAKE3.hash Nothing [T.encodeUtf8 content]
            }
    Left err -> throwError [AnySaraError err]

-- | Validate SEO properties.
validateSEO :: Item 'Unvalidated -> SaraM (Item 'Validated)
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
--   Automatically tracks dependencies.
loadData :: FilePath -> SaraM Aeson.Value
loadData path = do
  liftIO $ do
    content <- BS.readFile path
    let ext = takeExtension path
    case ext of
      ".json" -> case Aeson.decodeStrict content of
                   Just v -> return v
                   Nothing -> error $ "Failed to parse JSON: " ++ path
      ".yaml" -> case Yaml.decodeEither' content of
                   Right v -> return v
                   Left err -> error $ "Failed to parse YAML: " ++ path ++ ": " ++ show err
      _       -> error $ "Unsupported data format: " ++ ext

-- | Generates a Base64 LQIP magic token for an image.
imagePlaceholder :: FilePath -> SaraM Text
imagePlaceholder path = return $ "__LQIP__:" <> T.pack path <> "__"

-- | Smart constructor for regex routes.
regexRoute :: Text -> Text -> SaraM (Route 'Abstract)
regexRoute pat repl = case REngine.regexRoute pat repl of
  Right r -> return r
  Left err -> throwError [AnySaraError err]

-- | Convenience helper for glob patterns.
glob :: Text -> GlobPattern
glob = GlobPattern

void :: Functor f => f a -> f ()
void = fmap (const ())
