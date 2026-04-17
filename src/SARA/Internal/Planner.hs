{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module SARA.Internal.Planner
  ( planRules
  ) where

import Development.Shake
import Development.Shake.Classes
import Development.Shake.FilePath
import SARA.Monad (RuleDecl(..), SaraEnv(..), SaraM(..))
import SARA.Types (Item(..), AssetKind(..), SomeAssetKind(..), GlobPattern(..), FeedConfig(..), ValidationState(..))
import SARA.Config (SaraConfig(..))
import SARA.Security.PathGuard (guardPath, unSafePath)
import SARA.Security.ShellGuard (validatePath)
import SARA.Security.GlobGuard (unGlobPattern)
import SARA.Template.Renderer (renderTemplate)
import SARA.Internal.Hash (needBlake3, askLQIP)
import SARA.Error (AnySaraError(..), renderAnyErrorColor)
import SARA.SEO.Audit (auditRenderedHTML, AuditResult(..))
import SARA.Validator.LinkChecker (checkInternalLinks)
import SARA.Asset.Discover (inferAssetKind)
import SARA.Asset.Image (processImage)
import SARA.Search.Index (generatePartialIndex, mergePartialIndexes, mkSearchEntry)
import SARA.SEO.Sitemap (generateSitemap)
import SARA.SEO.Feed (generateRSS)

import GHC.Generics (Generic)
import Control.Monad (forM_, void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashSet as HS
import qualified Data.Map.Strict as Map
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import System.IO (hFlush, stdout)
import System.FilePath.Glob (globDir1, compile, match)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

collectOutputs :: SaraEnv -> [RuleDecl] -> [FilePath]
collectOutputs env decls =
  let outDir = cfgOutputDirectory (envConfig env)
  in [ outDir </> p | decl <- decls, p <- declToOutput decl ]
  where
    declToOutput = \case
      RuleRender _ _ outPath -> [outPath]
      RuleRenderRaw _ _ outPath -> [outPath]
      RuleSearch outPath _ -> [outPath]
      RulePartialSearch outPath _ -> [".cache" </> outPath]
      RuleSitemap outPath _ -> [outPath]
      RuleRSS outPath _ _ -> [outPath]
      _ -> []

-- | Translate RuleDecls from DSL into Shake Rules.
planRules :: SaraEnv -> [RuleDecl] -> Rules ()
planRules env decls = do
  addItemOracle env
  -- Collect all output paths from decls to create a 'want' rule
  let allOutputs = collectOutputs env decls
  want allOutputs

  -- Default rule to ensure output directory exists
  let outDir = cfgOutputDirectory (envConfig env)
  outDir %> \out -> do
    liftIO $ createDirectoryIfMissing True out

  mapM_ (translateDecl env) decls

translateDecl :: SaraEnv -> RuleDecl -> Rules ()
translateDecl env = \case
  RuleMatch g f        -> genMatch env g f
  RuleDiscover g       -> genDiscover env g
  RuleRender t i o     -> genRender env t i o
  RuleRenderRaw h i o  -> genRenderRaw env h i o
  RuleRemap _          -> return ()
  RuleSearch o ps      -> genSearch env o ps
  RulePartialSearch o i -> genPartialSearch env o i
  RuleSitemap o ps     -> genSitemap env o ps
  RuleRSS o cfg ps     -> genRSS env o cfg ps
  RuleDataDependency p -> genDataDependency env p
  RuleGlobal _         -> return ()

genMatch :: SaraEnv -> GlobPattern -> (FilePath -> SaraM (Item 'Validated)) -> Rules ()
genMatch _ _ _ = return () -- No-op: match logic already executed in DSL phase to collect rules.

genDiscover :: SaraEnv -> GlobPattern -> Rules ()
genDiscover env g = do
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  let outDir = cfgOutputDirectory (envConfig env)
  forM_ files $ \src -> do
    case guardPath (envRoot env) src of
      Left err -> liftIO $ TIO.putStrLn (renderAnyErrorColor (AnySaraError err))
      Right safeSrc -> do
        let out = outDir </> src
        liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert src sg, ())
        case inferAssetKind src of
          SomeAssetKind (ImageAsset spec) -> do
            out %> \o -> do
              need [unSafePath safeSrc]
              needBlake3 [unSafePath safeSrc]
              validatePath (unSafePath safeSrc)
              validatePath o
              processImage spec safeSrc o
          _ -> do
            out %> \o -> do
              need [unSafePath safeSrc]
              needBlake3 [unSafePath safeSrc]
              validatePath (unSafePath safeSrc)
              validatePath o
              copyFile' (unSafePath safeSrc) o

genRender :: SaraEnv -> FilePath -> Item 'Validated -> FilePath -> Rules ()
genRender env tplPath item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert outPath sg, ())

  fullOutPath %> \o -> do
    liftIO $ do
      putStr $ "\r\ESC[2K  [RENDERING] " ++ outPath
      hFlush stdout
    -- Load the real item content
    realItem <- askItem env (itemPath item)
    case guardPath (envRoot env) (itemPath realItem) of
      Left err -> fail $ T.unpack (renderAnyErrorColor (AnySaraError err))
      Right safeSrc -> do
        case guardPath (envRoot env) tplPath of
          Left err -> fail $ T.unpack (renderAnyErrorColor (AnySaraError err))
          Right safeTpl -> do
            needBlake3 [unSafePath safeSrc, unSafePath safeTpl]
            
            let config = envConfig env
            let siteMeta = KM.fromList
                  [ (K.fromText "siteTitle", Aeson.String (cfgSiteTitle config))
                  , (K.fromText "siteUrl",   Aeson.String (cfgSiteUrl config))
                  , (K.fromText "siteAuthor", Aeson.String (cfgSiteAuthor config))
                  ]
            let itemWithBody = KM.insert (K.fromText "itemBody") (Aeson.String (itemBody realItem)) (itemMeta realItem)
            let ctx = Aeson.Object $ KM.union itemWithBody siteMeta
            
            renderTemplate (unSafePath safeTpl) ctx >>= \case
              Left err -> fail $ show err
              Right html -> do
                -- Inject real LQIPs by scanning for magic tokens
                finalHtml <- injectLQIPs html
                
                sg <- liftIO $ readIORef (envSiteGraph env)
                let linkIssues = checkInternalLinks sg (itemPath realItem) outPath finalHtml
                let seoResult = auditRenderedHTML outPath finalHtml
                case seoResult of
                  AuditIssues _ issues -> do
                    liftIO $ writeIORef (envHasErrors env) True
                    liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) (issues ++ linkIssues)
                  AuditPassed -> do
                    case linkIssues of
                      [] -> return ()
                      _  -> do
                        liftIO $ writeIORef (envHasErrors env) True
                        liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) linkIssues
                writeFile' o (T.unpack finalHtml)

genRenderRaw :: SaraEnv -> Text -> Item 'Validated -> FilePath -> Rules ()
genRenderRaw env html item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert outPath sg, ())

  fullOutPath %> \o -> do
    -- Load the real item content even if we use raw HTML (for metadata context)
    realItem <- askItem env (itemPath item)
    -- Inject real LQIPs even for raw HTML
    finalHtml <- injectLQIPs html
    
    sg <- liftIO $ readIORef (envSiteGraph env)
    let linkIssues = checkInternalLinks sg (itemPath realItem) outPath finalHtml
    let seoResult = auditRenderedHTML outPath finalHtml
    case seoResult of
      AuditIssues _ issues -> do
        liftIO $ writeIORef (envHasErrors env) True
        liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) (issues ++ linkIssues)
      AuditPassed -> do
        case linkIssues of
          [] -> return ()
          _  -> do
            liftIO $ writeIORef (envHasErrors env) True
            liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) linkIssues
    writeFile' o (T.unpack finalHtml)

injectLQIPs :: Text -> Action Text
injectLQIPs html = do
  let tokens = findLQIPTokens html
  replacements <- mapM (\t -> (t,) <$> askLQIP (T.unpack t)) tokens
  return $ foldr (\(token, b64) acc -> T.replace ("__LQIP__:" <> token <> "__") b64 acc) html replacements

findLQIPTokens :: Text -> [Text]
findLQIPTokens t = 
  let (beforeMatch, matchFound) = T.breakOn "__LQIP__:" t
  in if T.null matchFound
     then []
     else 
       let rest = T.drop 9 matchFound
           (path, after) = T.breakOn "__" rest
       in path : findLQIPTokens (T.drop 2 after)

genSearch :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSearch env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert outPath sg, ())
  
  -- Register partial search rules for each item
  forM_ items $ \i -> do
    genPartialSearch env (itemPath i ++ ".partial.json") i

  fullOutPath %> \o -> do
    let partials = map (\i -> outDir </> ".cache" </> (itemPath i ++ ".partial.json")) items
    need partials
    mergePartialIndexes partials o

genPartialSearch :: SaraEnv -> FilePath -> Item 'Validated -> Rules ()
genPartialSearch env outPath item = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> ".cache" </> outPath
  
  fullOutPath %> \o -> do
    -- During execution, load the real item
    realItem <- askItem env (itemPath item)
    let (entry, content) = mkSearchEntry realItem
    generatePartialIndex entry content o

genSitemap :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSitemap env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert outPath sg, ())
  
  fullOutPath %> \o -> do
    -- During execution, load real items
    realItems <- mapM (askItem env . itemPath) items
    generateSitemap (cfgSiteUrl (envConfig env)) realItems o

genRSS :: SaraEnv -> FilePath -> FeedConfig -> [Item 'Validated] -> Rules ()
genRSS env outPath cfg items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert outPath sg, ())
  
  fullOutPath %> \o -> do
    -- During execution, load real items
    realItems <- mapM (askItem env . itemPath) items
    generateRSS cfg realItems o

genDataDependency :: SaraEnv -> FilePath -> Rules ()
genDataDependency _ path = action $ need [path]

-- | Load a single item by its source path. Uses a cache and an Oracle for dependency tracking.
askItem :: SaraEnv -> FilePath -> Action (Item 'Validated)
askItem env path = do
  need [path]
  askOracle (ItemOracle path)
  cache <- liftIO $ readIORef (envItemCache env)
  case Map.lookup path cache of
    Just item -> return item
    Nothing -> fail $ "ItemOracle succeeded but cache is empty for " ++ path

newtype ItemOracle = ItemOracle FilePath
  deriving (Show, Typeable, Eq, Hashable, Binary, NFData, Generic)

type instance RuleResult ItemOracle = ()

addItemOracle :: SaraEnv -> Rules ()
addItemOracle env = void $ addOracle $ \(ItemOracle path) -> do
  need [path]
  cache <- liftIO $ readIORef (envItemCache env)
  if Map.member path cache
    then return ()
    else do
      decls <- liftIO $ readIORef (envRules env)
      let matchingCompilers = [ f | RuleMatch g f <- decls, matchGlob g path ]
      case matchingCompilers of
        (f:_) -> do
          -- Run 'f' in execution mode (envIsPlanning should be False)
          res <- liftIO $ runExceptT $ runReaderT (unSaraM (f path)) env
          case res of
            Right item -> liftIO $ atomicModifyIORef' (envItemCache env) (\c -> (Map.insert path item c, ()))
            Left errs -> fail $ T.unpack $ T.unlines $ map renderAnyErrorColor errs
        [] -> fail $ "No compiler found for " ++ path

matchGlob :: GlobPattern -> FilePath -> Bool
matchGlob (GlobPattern p) path = match (compile (T.unpack p)) path
