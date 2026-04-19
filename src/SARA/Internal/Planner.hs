{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module SARA.Internal.Planner
  ( planRules
  , askItem
  ) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import SARA.Monad (RuleDecl(..), SaraEnv(..), SaraM(..), SaraState(..), SPath)
import SARA.Types (Item(..), AssetKind(..), SomeAssetKind(..), GlobPattern(..), FeedConfig(..), ValidationState(..), Route(..), Pager(..), unSafePath)
import qualified SARA.Routing.Engine as REngine
import SARA.Config (SaraConfig(..))
import SARA.Security.PathGuard (guardPath)
import SARA.Security.ShellGuard (validatePath)
import SARA.Template.Renderer (renderTemplate)
import SARA.Internal.Hash (needBlake3, askLQIP)
import SARA.Error (AnySaraError(..), renderAnyErrorColor, renderErrorColor, errorDetails)
import SARA.SEO.Audit (auditRenderedHTML, AuditResult(..))
import SARA.Validator.LinkChecker (checkInternalLinks)
import SARA.Validator.AssetChecker (checkAssetReferences)
import SARA.Asset.Discover (inferAssetKind)
import SARA.Asset.Image (processImage)
import SARA.Search.Index (generatePartialIndex, mergePartialIndexes, mkSearchEntry)
import SARA.SEO.Sitemap (generateSitemap)
import SARA.SEO.Feed (generateRSS)

import GHC.Generics (Generic)
import Control.Monad (forM_, void, unless)
import Control.Monad.Reader (runReaderT)
import Data.Maybe (maybeToList)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import qualified Data.ByteString as BS
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import qualified Data.List as L
import UnliftIO.IORef (atomicModifyIORef', readIORef)
import System.FilePath.Glob (globDir1, compile, match)
import System.Directory (createDirectoryIfMissing, canonicalizePath, renameFile)
import System.IO (hFlush, stdout, openTempFile, hClose)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified UnliftIO.Exception as E

collectOutputs :: SaraEnv -> [RuleDecl] -> IO [FilePath]
collectOutputs env decls = do
  let outDir = cfgOutputDirectory (envConfig env)
  state <- readIORef (envState env)
  -- 1. Paths from items matching 'match' globs
  let itemOutputs = [ T.unpack p | item <- Map.elems (stateItemCache state), ResolvedRoute p <- [itemRoute item] ]
  -- 2. Paths from explicit render rules (which might be the same as #1)
  let ruleOutputs = concat [ declToOutput decl | decl <- decls ]
  let reportFile = maybeToList (cfgSEOReportPath (envConfig env))
  
  return $ map (outDir </>) (L.nub $ itemOutputs ++ map T.unpack ruleOutputs ++ reportFile)
  where
    declToOutput = \case
      RuleRender _ _ outPath -> [outPath]
      RuleRenderPager _ _ outPath -> [outPath]
      RuleRenderRaw _ _ outPath -> [outPath]
      RuleSearch outPath _ -> [outPath]
      RulePartialSearch outPath _ -> [T.pack (".cache" </> T.unpack outPath)]
      RuleSitemap outPath _ -> [outPath]
      RuleRSS outPath _ _ -> [outPath]
      _ -> []

-- | Translate RuleDecls from DSL into Shake Rules.
planRules :: SaraEnv -> [RuleDecl] -> Rules ()
planRules env decls = do
  addItemOracle env
  -- Collect all output paths from decls to create a 'want' rule
  allOutputs <- liftIO $ collectOutputs env decls
  want allOutputs

  -- Default rule to ensure output directory exists
  let outDir = cfgOutputDirectory (envConfig env)
  outDir %> \out -> do
    liftIO $ createDirectoryIfMissing True out

  -- Industrial Fix for Double-Build: Deduplicate discover rules
  let discoverGlobs = [ g | RuleDiscover g <- decls ]
  genAllDiscover env discoverGlobs

  genSEOReport env

  mapM_ (translateDecl env) [ d | d <- decls, not (isDiscover d) ]
  where
    isDiscover (RuleDiscover _) = True
    isDiscover _                = False

translateDecl :: SaraEnv -> RuleDecl -> Rules ()
translateDecl env = \case
  RuleMatch g f        -> genMatch env g f
  RuleDiscover _       -> return () -- Handled by genAllDiscover
  RuleRender t i o     -> genRender env t i o
  RuleRenderPager t p o -> genRenderPager env t p o
  RuleRenderRaw h i o  -> genRenderRaw env h i o
  RuleRemap _          -> return ()
  RuleSearch o ps      -> genSearch env o ps
  RulePartialSearch o i -> genPartialSearch env o i
  RuleSitemap o ps     -> genSitemap env o ps
  RuleRSS o cfg ps     -> genRSS env o cfg ps
  RuleGlobal _         -> return ()

-- | Industrial Grade Discover: Deduplicates files across multiple globs efficiently.
genAllDiscover :: SaraEnv -> [GlobPattern] -> Rules ()
genAllDiscover env gs = do
  allFiles <- liftIO $ do
    fss <- mapM (\(GlobPattern g) -> globDir1 (compile (T.unpack g)) ".") gs
    return $ HS.toList $ HS.fromList (concat fss)
  let outDir = cfgOutputDirectory (envConfig env)
  forM_ allFiles $ \src -> do
    res <- liftIO $ guardPath (envRoot env) src
    case res of
      Left err -> liftIO $ TIO.putStrLn (renderAnyErrorColor (AnySaraError err))
      Right safeSrc -> do
        let out = outDir </> src
        liftIO $ atomicModifyIORef' (envState env) $ \s ->
          (s { stateSiteGraph = HS.insert (T.pack src) (stateSiteGraph s) }, ())
        case inferAssetKind (envConfig env) (T.pack src) of
          SomeAssetKind (ImageAsset spec) -> do
            out %> \o -> do
              need [unSafePath safeSrc]
              needBlake3 [unSafePath safeSrc]
              validatePath (unSafePath safeSrc)
              validatePath o
              unless (cfgDryRun (envConfig env)) $
                processImage (cfgAllowedCommands (envConfig env)) spec safeSrc o
          _ -> do
            out %> \o -> do
              need [unSafePath safeSrc]
              needBlake3 [unSafePath safeSrc]
              validatePath (unSafePath safeSrc)
              validatePath o
              unless (cfgDryRun (envConfig env)) $
                copyFile' (unSafePath safeSrc) o

genMatch :: SaraEnv -> GlobPattern -> (SPath -> SaraM (Item 'Validated)) -> Rules ()
genMatch env g _f = do
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  let outDir = cfgOutputDirectory (envConfig env)
  
  -- We need to know the output path for EACH file.
  -- These were already computed during the planning phase and stored in stateItemCache.
  state <- liftIO $ readIORef (envState env)
  let cache = stateItemCache state
  
  forM_ files $ \src -> do
    case Map.lookup (T.pack src) cache of
      Just item -> do
        -- If item has UnresolvedRoute, resolve it to default SlugRoute now
        resolvedPath' <- case itemRoute item of
          ResolvedRoute p -> return (Just p)
          UnresolvedRoute -> do
            -- Try resolving with SlugRoute
            res <- liftIO $ REngine.resolveRoute SlugRoute src
            case res of
              Right (ResolvedRoute p) -> return (Just p)
              _ -> return Nothing
        
        case resolvedPath' of
          Just outPath -> do
            let fullOutPath = outDir </> T.unpack outPath
            
            -- Industrial fix for C-01: 
            -- Only register a default 'match' rule if no 'render' rule exists for this path.
            -- This prevents Shake rule collisions while ensuring every matched file is built.
            let hasRenderRule = any (isRenderFor outPath) (stateRules state)
            
            unless hasRenderRule $ do
              fullOutPath %> \o -> do
                -- Execute the user's compiler function for this specific file
                realItem <- askItem env (T.pack src)
                -- Write the rendered body (skip if dry run)
                unless (cfgDryRun (envConfig env)) $ do
                  liftIO $ createDirectoryIfMissing True (takeDirectory o)
                  liftIO $ atomicWriteFile o (itemBody realItem)
          Nothing -> return ()
      Nothing -> return ()
  where
    isRenderFor outPath (RuleRender _ _ p) = p == outPath
    isRenderFor outPath (RuleRenderRaw _ _ p) = p == outPath
    isRenderFor _ _ = False

-- | Atomic write: writes to temp file then renames.
--   Industrial Grade: Enforces UTF-8 encoding regardless of system locale.
atomicWriteFile :: FilePath -> Text -> IO ()
atomicWriteFile path content = do
  let dir = takeDirectory path
  createDirectoryIfMissing True dir
  (tmpPath, h) <- openTempFile dir ".sara-tmp"
  -- Enforce UTF-8 via ByteString to bypass system locale issues
  BS.hPut h (TE.encodeUtf8 content)
  hClose h
  renameFile tmpPath path

genRender :: SaraEnv -> SPath -> Item 'Validated -> SPath -> Rules ()
genRender env tplPath item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())

  fullOutPath %> \o -> do
    liftIO $ do
      putStr $ "\r\ESC[2K  [RENDERING] " ++ T.unpack outPath
      hFlush stdout
    -- Load the real item content
    realItem <- askItem env (itemPath item)
    res <- liftIO $ guardPath (envRoot env) (T.unpack $ itemPath realItem)
    case res of
      Left err -> fail $ T.unpack (renderAnyErrorColor (AnySaraError err))
      Right _safeSrc -> do
        absTpl <- liftIO $ canonicalizePath (T.unpack tplPath)
        let safeTplPath = T.pack absTpl
        state <- liftIO $ readIORef (envState env)
        case Map.lookup safeTplPath (stateTemplateCache state) of
          Just _mvar -> do
            let config = envConfig env
            let siteMeta = KM.fromList
                  [ (K.fromText "siteTitle", Aeson.String (cfgSiteTitle config))
                  , (K.fromText "siteUrl",   Aeson.String (cfgSiteUrl config))
                  , (K.fromText "siteAuthor", Aeson.String (cfgSiteAuthor config))
                  ]
            let itemWithBody = KM.insert (K.fromText "itemBody") (Aeson.String (itemBody realItem)) (itemMeta realItem)
            let ctx = Aeson.Object $ KM.union itemWithBody siteMeta
            
            renderTemplate env (T.unpack tplPath) ctx >>= \case
              Left err -> fail $ T.unpack $ renderErrorColor err
              Right html -> do
                -- Inject real LQIPs by scanning for magic tokens
                finalHtml <- injectLQIPs env html
                
                state' <- liftIO $ readIORef (envState env)
                let graph = stateSiteGraph state'
                let linkIssues = checkInternalLinks graph (itemPath realItem) outPath finalHtml
                let assetIssues = checkAssetReferences graph (itemPath realItem) outPath finalHtml
                let allIssues = linkIssues ++ assetIssues
                
                let seoResult = auditRenderedHTML (T.unpack outPath) finalHtml
                case seoResult of
                  AuditIssues p issues -> do
                    recordIssues env (issues ++ allIssues) p
                    liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) (issues ++ allIssues)
                  AuditPassed -> do
                    recordIssues env allIssues (T.unpack outPath)
                    unless (null allIssues) $ 
                      liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) allIssues
                -- Write the rendered file (skip if dry run)
                unless (cfgDryRun (envConfig env)) $ do
                  liftIO $ createDirectoryIfMissing True (takeDirectory o)
                  liftIO $ atomicWriteFile o finalHtml
          Nothing -> fail $ "Template not found in cache: " ++ T.unpack safeTplPath

genRenderPager :: SaraEnv -> SPath -> Pager 'Validated -> SPath -> Rules ()
genRenderPager env tplPath pager outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())

  fullOutPath %> \o -> do
    absTpl <- liftIO $ canonicalizePath (T.unpack tplPath)
    let safeTplPath = T.pack absTpl
    state <- liftIO $ readIORef (envState env)
    case Map.lookup safeTplPath (stateTemplateCache state) of
      Just _mvar -> do
        let config = envConfig env
        let siteMeta = KM.fromList
              [ (K.fromText "siteTitle", Aeson.String (cfgSiteTitle config))
              , (K.fromText "siteUrl",   Aeson.String (cfgSiteUrl config))
              , (K.fromText "siteAuthor", Aeson.String (cfgSiteAuthor config))
              ]
        let ctx = Aeson.Object $ KM.insert (K.fromText "pager") (Aeson.toJSON pager) siteMeta
        
        renderTemplate env (T.unpack tplPath) ctx >>= \case
          Left err -> fail $ T.unpack $ renderErrorColor err
          Right html -> do
            finalHtml <- injectLQIPs env html
            
            -- Validation
            state' <- liftIO $ readIORef (envState env)
            let graph = stateSiteGraph state'
                linkIssues = checkInternalLinks graph "" outPath finalHtml -- No source path for virtual pager
                assetIssues = checkAssetReferences graph "" outPath finalHtml
                allIssues = linkIssues ++ assetIssues
            
            let seoResult = auditRenderedHTML (T.unpack outPath) finalHtml
            case seoResult of
              AuditIssues p issues -> do
                recordIssues env (issues ++ allIssues) p
                liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) (issues ++ allIssues)
              AuditPassed -> do
                recordIssues env allIssues (T.unpack outPath)
                unless (null allIssues) $ 
                  liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) allIssues
            unless (cfgDryRun (envConfig env)) $ do
              liftIO $ createDirectoryIfMissing True (takeDirectory o)
              liftIO $ atomicWriteFile o finalHtml
      Nothing -> fail $ "Template not found in cache: " ++ T.unpack safeTplPath

genRenderRaw :: SaraEnv -> Text -> Item 'Validated -> SPath -> Rules ()
genRenderRaw env html item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())

  fullOutPath %> \o -> do
    -- Load the real item content even if we use raw HTML (for metadata context)
    realItem <- askItem env (itemPath item)
    -- Inject real LQIPs even for raw HTML
    finalHtml <- injectLQIPs env html
    
    state <- liftIO $ readIORef (envState env)
    let graph = stateSiteGraph state
    let linkIssues = checkInternalLinks graph (itemPath realItem) outPath finalHtml
    let assetIssues = checkAssetReferences graph (itemPath realItem) outPath finalHtml
    let allIssues = linkIssues ++ assetIssues

    let seoResult = auditRenderedHTML (T.unpack outPath) finalHtml
    case seoResult of
      AuditIssues p issues -> do
        recordIssues env (issues ++ allIssues) p
        liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) (issues ++ allIssues)
      AuditPassed -> do
        recordIssues env allIssues (T.unpack outPath)
        unless (null allIssues) $ 
          liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) allIssues
        -- Skip writing in dry run
        unless (cfgDryRun (envConfig env)) $ do
          liftIO $ createDirectoryIfMissing True (takeDirectory o)
          liftIO $ atomicWriteFile o finalHtml
injectLQIPs :: SaraEnv -> Text -> Action Text
injectLQIPs _env html = do
  let tokens = findLQIPTokens html
  replacements <- mapM (\t -> (t,) <$> askLQIP (T.unpack t)) tokens
  return $ foldr (\(token, b64) acc -> T.replace ("{{SARA_LQIP:" <> token <> "}}") b64 acc) html replacements

findLQIPTokens :: Text -> [Text]
findLQIPTokens t = 
  let (_beforeMatch, matchFound) = T.breakOn "{{SARA_LQIP:" t
  in if T.null matchFound then []
     else 
       let rest = T.drop 12 matchFound
           (path, after) = T.breakOn "}}" rest
       in path : findLQIPTokens (T.drop 2 after)

genSearch :: SaraEnv -> SPath -> [Item 'Validated] -> Rules ()
genSearch env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())
  
  -- Register partial search rules for each item
  forM_ items $ \i -> do
    genPartialSearch env (itemPath i <> ".partial.json") i

  fullOutPath %> \o -> do
    let partials = map (\i -> outDir </> ".cache" </> T.unpack (itemPath i <> ".partial.json")) items
    need partials
    unless (cfgDryRun (envConfig env)) $
      mergePartialIndexes partials o

genPartialSearch :: SaraEnv -> SPath -> Item 'Validated -> Rules ()
genPartialSearch env outPath item = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> ".cache" </> T.unpack outPath
  
  fullOutPath %> \o -> do
    -- During execution, load the real item
    realItem <- askItem env (itemPath item)
    let (entry, content) = mkSearchEntry realItem
    unless (cfgDryRun (envConfig env)) $
      generatePartialIndex entry content o

genSitemap :: SaraEnv -> SPath -> [Item 'Validated] -> Rules ()
genSitemap env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())
  
  fullOutPath %> \o -> do
    -- During execution, load real items
    realItems <- mapM (askItem env . itemPath) items
    unless (cfgDryRun (envConfig env)) $
      liftIO $ generateSitemap (cfgSiteUrl (envConfig env)) realItems o

genRSS :: SaraEnv -> SPath -> FeedConfig -> [Item 'Validated] -> Rules ()
genRSS env outPath cfg items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> T.unpack outPath
  liftIO $ atomicModifyIORef' (envState env) $ \s ->
    (s { stateSiteGraph = HS.insert outPath (stateSiteGraph s) }, ())
  
  fullOutPath %> \o -> do
    -- During execution, load real items
    realItems <- mapM (askItem env . itemPath) items
    unless (cfgDryRun (envConfig env)) $
      liftIO $ generateRSS cfg realItems o

-- | Load a single item by its source path. Uses a cache and an Oracle for dependency tracking.
askItem :: SaraEnv -> SPath -> Action (Item 'Validated)
askItem env path = do
  _ <- askOracle (ItemOracle (T.unpack path))
  state <- liftIO $ readIORef (envState env)
  case Map.lookup path (stateItemCache state) of
    Just item -> return item
    Nothing   -> fail $ "Planner internal error: Item not in cache " ++ T.unpack path

newtype ItemOracle = ItemOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult ItemOracle = [SPath]

addItemOracle :: SaraEnv -> Rules ()
addItemOracle env = void $ addOracle $ \(ItemOracle path) -> do
  need [path]
  state <- liftIO $ readIORef (envState env)
  let pathText = T.pack path
  
  -- Clear current deps before running compiler
  liftIO $ atomicModifyIORef' (envState env) $ \s -> (s { stateCurrentDeps = [] }, ())
  
  -- Find the compiler rule
  let matchingRules = [ f | RuleMatch g f <- stateRules state, matchGlob g path ]
  case matchingRules of
    (f:_) -> do
      -- Run in execution mode
      itemRes <- liftIO $ E.try (runReaderT (unSaraM (f pathText)) env { envIsPlanning = False })
      case itemRes of
        Left (err :: E.SomeException) -> fail (show err)
        Right item -> do
          -- Record item and get collected dependencies
          deps <- liftIO $ atomicModifyIORef' (envState env) $ \s ->
            (s { stateItemCache = Map.insert pathText item (stateItemCache s) }, stateCurrentDeps s)
          
          -- Tell Shake we need these dependencies
          need (map T.unpack deps)
          return deps
    [] -> do
      -- Fallback if no matching rule (should not happen for items being asked)
      return []

matchGlob :: GlobPattern -> FilePath -> Bool
matchGlob (GlobPattern p) path = match (compile (T.unpack p)) path

-- | Generates a machine-readable SEO report for the entire site.
genSEOReport :: SaraEnv -> Rules ()
genSEOReport env = do
  case cfgSEOReportPath (envConfig env) of
    Nothing -> return ()
    Just relPath -> do
      let outDir = cfgOutputDirectory (envConfig env)
      let fullPath = outDir </> relPath
      
      fullPath %> \o -> do
        state <- liftIO $ readIORef (envState env)
        let issues = stateSEOIssues state
        unless (cfgDryRun (envConfig env)) $ do
          liftIO $ createDirectoryIfMissing True (takeDirectory o)
          liftIO $ Aeson.encodeFile o issues

-- | Industrial Grade Issue Tracking: Classifies and records issues by severity and type.
recordIssues :: SaraEnv -> [AnySaraError] -> FilePath -> Action ()
recordIssues env issues path = do
  liftIO $ forM_ issues $ \(AnySaraError err) -> do
    let (level, _, _, _) = errorDetails err
    atomicModifyIORef' (envState env) $ \s ->
      let s1 = if level == "seo" 
               then s { stateHasSEOIssues = True, stateSEOIssues = Map.insertWith (++) path [AnySaraError err] (stateSEOIssues s) }
               else if level == "security"
               then s { stateHasSecurityIssues = True, stateHasErrors = True }
               else s { stateHasErrors = True }
      in (s1, ())
