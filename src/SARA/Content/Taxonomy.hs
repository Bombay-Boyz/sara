{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}

-- | Taxonomies — tags, categories, or any other metadata field a site
--   wants to group content by, with one listing page rendered per
--   distinct term (e.g. @\/tags\/haskell\/index.html@).
--
--   Implemented entirely in terms of DSL primitives that already
--   exist ('SARA.DSL.render', 'SARA.Routing.Engine.resolveRoute') —
--   deliberately not a new 'SARA.Monad.RuleDecl' constructor. A
--   taxonomy page is a rendered page like any other: it doesn't need
--   its own code path through 'SARA.Internal.Planner', just a
--   synthetic 'Item' whose metadata is the term and its matching
--   posts. Routing every generated path through 'resolveRoute' (rather
--   than constructing 'ResolvedRoute' directly) also means the
--   Windows-path-safety check added earlier this session applies here
--   too — a tag literally named @CON@ or containing @?@ is exactly
--   the kind of "arbitrary user-supplied text ending up in a path"
--   case that check exists for.
module SARA.Content.Taxonomy
  ( slugify
  , extractTerms
  , groupByTerm
  , buildTaxonomyPages
  ) where

import SARA.Types (ItemP(..), ValidationState(..))
import SARA.Monad (SaraM)
import SARA.DSL (renderSyntheticPage)
import SARA.Error (SaraError(..), AnySaraError(..))
import SARA.Content.Summary (itemToSummary)
import Control.Monad.Except (throwError)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Char (isAlphaNum, toLower)
import Data.List (foldl')
import System.FilePath ((</>))

-- | Lowercased, non-alphanumeric runs collapsed to a single hyphen,
--   leading\/trailing hyphens trimmed. Total for any input, including
--   the empty string (which slugifies to empty — 'buildTaxonomyPages'
--   is what decides whether an empty slug is acceptable, not this
--   function, keeping the two concerns — "make text URL-safe" and
--   "is this a usable path segment" — separate).
slugify :: Text -> Text
slugify =
  T.dropAround (== '-')
    . collapseHyphens
    . T.map (\c -> if isAlphaNum c then toLower c else '-')
  where
    collapseHyphens = T.pack . dedupe . T.unpack
    dedupe (a:b:rest) | a == '-' && b == '-' = dedupe (b : rest)
    dedupe (c:rest)   = c : dedupe rest
    dedupe []         = []

-- | Every term listed under 'fieldName' in an item's metadata. Accepts
--   either a JSON array of strings (@tags: [a, b]@) or a single bare
--   string (@category: a@) — both are common frontmatter shapes across
--   the ecosystems SARA migrates from. Any other shape (numbers,
--   objects, absence of the field) yields no terms for that item,
--   rather than a partial or guessed extraction.
extractTerms :: Text -> ItemP v Aeson.Object -> [Text]
extractTerms fieldName item =
  case KM.lookup (K.fromText fieldName) (itemMeta item) of
    Just (Aeson.Array vs) -> [ s | Aeson.String s <- V.toList vs ]
    Just (Aeson.String s) -> [s]
    _                     -> []

-- | Groups items by every term found under 'fieldName', so an item
--   with @tags: [a, b]@ appears under both @a@ and @b@'s lists.
groupByTerm :: Text -> [ItemP 'Validated Aeson.Object] -> Map Text [ItemP 'Validated Aeson.Object]
groupByTerm fieldName items =
  foldl' insertItem Map.empty [ (term, item) | item <- items, term <- extractTerms fieldName item ]
  where
    insertItem acc (term, item) = Map.insertWith (++) term [item] acc

-- | Renders one listing page per distinct term found under
--   'fieldName', to @outDirBase\/\<slug\>\/index.html@, using
--   'template' with @term@ (the original, un-slugified term text) and
--   @posts@ (each item's 'SARA.Content.Summary.itemToSummary') in
--   scope — the same @{{#posts}}@ shape 'templates/index.html' already
--   expects.
--
--   Returns the synthetic per-term items that were rendered, so a
--   caller can fold them into 'SARA.DSL.buildSitemap' alongside real
--   content pages if they want taxonomy pages listed there too.
--
--   Fails outright (via 'throwError', not a partial write) if two
--   distinct terms slugify to the same value (e.g. "C++" and "C--"
--   both becoming "c") — silently letting the second term's page
--   overwrite the first's would be exactly the kind of silent data
--   loss this codebase's error-handling audit exists to prevent.
buildTaxonomyPages
  :: Text                                    -- ^ metadata field to group by, e.g. "tags"
  -> FilePath                                -- ^ template to render each term page with
  -> FilePath                                -- ^ output directory base, e.g. "tags"
  -> [ItemP 'Validated Aeson.Object]
  -> SaraM [ItemP 'Validated Aeson.Object]
buildTaxonomyPages fieldName template outDirBase items = do
  let grouped = groupByTerm fieldName items
  checkNoSlugCollisions (Map.keys grouped)
  mapM (uncurry (renderTermPage template outDirBase)) (Map.toList grouped)

-- | Every term slugifies to a distinct path segment, or this fails
--   naming the colliding terms — see 'buildTaxonomyPages'.
checkNoSlugCollisions :: [Text] -> SaraM ()
checkNoSlugCollisions terms =
  let bySlug = Map.fromListWith (++) [ (slugify t, [t]) | t <- terms ]
      collisions = Map.filter (\ts -> length ts > 1) bySlug
  in if Map.null collisions
     then pure ()
     else throwError
       [ AnySaraError (RouteConflict
           (T.unpack (T.intercalate ", " ts))
           ""
           (T.unpack slug))
       | (slug, ts) <- Map.toList collisions
       ]

renderTermPage
  :: FilePath
  -> FilePath
  -> Text
  -> [ItemP 'Validated Aeson.Object]
  -> SaraM (ItemP 'Validated Aeson.Object)
renderTermPage template outDirBase term termItems =
  renderSyntheticPage template outPath meta (TE.encodeUtf8 term)
  where
    outPath = outDirBase </> T.unpack (slugify term) </> "index.html"
    meta = KM.fromList
      [ (K.fromText "term", Aeson.String term)
      , (K.fromText "posts", Aeson.Array (V.fromList (map (itemToSummary 200) termItems)))
      ]
