{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TupleSections #-}

module SARA.Internal.Planner
  ( planRules
  ) where

import Development.Shake
import Development.Shake.FilePath
import SARA.Monad (RuleDecl(..), SaraEnv(..), SaraM(..))
import SARA.Types (Item(..), AssetKind(..), SomeAssetKind(..), GlobPattern, ValidationState(..), FeedConfig(..))
import SARA.Config (SaraConfig(..))
import SARA.Security.PathGuard (guardPath, unSafePath)
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

import Control.Monad (forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashSet as HS
import Data.IORef (atomicModifyIORef', readIORef, writeIORef)
import System.IO (hFlush, stdout)
import System.FilePath.Glob (globDir1, compile)
import System.Directory (createDirectoryIfMissing)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K

collectOutputs :: SaraEnv -> [RuleDecl] -> [FilePath]
collectOutputs env decls =
  let outDir = cfgOutputDirectory (envConfig env)
      go = \case
        [] -> []
        (RuleRender _ _ outPath : xs) -> (outDir </> outPath) : go xs
        (RuleRenderRaw _ _ outPath : xs) -> (outDir </> outPath) : go xs
        (RuleMatch _ _ : xs) -> go xs
        (RuleDiscover _ : xs) -> go xs
        (RuleRemap _ : xs) -> go xs
        (RuleSearch outPath _ : xs) -> (outDir </> outPath) : go xs
        (RulePartialSearch outPath _ : xs) -> (outDir </> ".cache" </> outPath) : go xs
        (RuleSitemap outPath _ : xs) -> (outDir </> outPath) : go xs
        (RuleRSS outPath _ _ : xs) -> (outDir </> outPath) : go xs
        (RuleGlobal _ : xs) -> go xs
  in go decls

-- | Translate RuleDecls from DSL into Shake Rules.
planRules :: SaraEnv -> [RuleDecl] -> Rules ()
planRules env decls = do
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
  RuleSearch o items   -> genSearch env o items
  RulePartialSearch o i -> genPartialSearch env o i
  RuleSitemap o items  -> genSitemap env o items
  RuleRSS o cfg items  -> genRSS env o cfg items
  RuleGlobal globalAction -> genGlobal env globalAction

genMatch :: SaraEnv -> GlobPattern -> (FilePath -> SaraM ()) -> Rules ()
genMatch env g f = do
  let patStr = T.unpack (unGlobPattern g)
  files <- liftIO $ globDir1 (compile patStr) "."
  forM_ files $ \src -> do
    liftIO (runExceptT $ runWriterT $ runReaderT (unSaraM (f src)) env) >>= \case
      Left errs -> liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
      Right ((), nestedDecls) -> do
        let nestedOutputs = collectOutputs env nestedDecls
        want nestedOutputs
        mapM_ (translateDecl env) nestedDecls

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
        liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert out sg, ())
        case inferAssetKind src of
          SomeAssetKind (ImageAsset spec) -> do
            out %> \o -> do
              needBlake3 [unSafePath safeSrc]
              processImage spec safeSrc o
          _ -> do
            out %> \o -> do
              needBlake3 [unSafePath safeSrc]
              copyFile' (unSafePath safeSrc) o

genRender :: SaraEnv -> FilePath -> Item 'Validated -> FilePath -> Rules ()
genRender env tplPath item outPath = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert fullOutPath sg, ())

  fullOutPath %> \o -> do
    liftIO $ do
      putStr $ "\r\ESC[2K  [RENDERING] " ++ outPath
      hFlush stdout
    case guardPath (envRoot env) (itemPath item) of
      Left err -> error $ T.unpack (renderAnyErrorColor (AnySaraError err))
      Right safeSrc -> do
        case guardPath (envRoot env) tplPath of
          Left err -> error $ T.unpack (renderAnyErrorColor (AnySaraError err))
          Right safeTpl -> do
            needBlake3 [unSafePath safeSrc, unSafePath safeTpl]
            
            let config = envConfig env
            let siteMeta = KM.fromList
                  [ (K.fromText "siteTitle", Aeson.String (cfgSiteTitle config))
                  , (K.fromText "siteUrl",   Aeson.String (cfgSiteUrl config))
                  , (K.fromText "siteAuthor", Aeson.String (cfgSiteAuthor config))
                  ]
            let itemWithBody = KM.insert (K.fromText "itemBody") (Aeson.String (itemBody item)) (itemMeta item)
            let ctx = Aeson.Object $ KM.union itemWithBody siteMeta
            
            renderTemplate (unSafePath safeTpl) ctx >>= \case
              Left err -> error $ show err
              Right html -> do
                -- Inject real LQIPs by scanning for magic tokens
                finalHtml <- injectLQIPs html
                
                sg <- liftIO $ readIORef (envSiteGraph env)
                let linkIssues = checkInternalLinks sg (itemPath item) outPath finalHtml
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
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert fullOutPath sg, ())

  fullOutPath %> \o -> do
    -- Inject real LQIPs even for raw HTML
    finalHtml <- injectLQIPs html
    
    sg <- liftIO $ readIORef (envSiteGraph env)
    let linkIssues = checkInternalLinks sg (itemPath item) outPath finalHtml
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
  let (_, match) = T.breakOn "__LQIP__:" t
  in if T.null match
     then []
     else 
       let rest = T.drop 8 match
           (path, after) = T.breakOn "__" rest
       in path : findLQIPTokens (T.drop 2 after)

genSearch :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSearch env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert fullOutPath sg, ())
  
  let partials = map (\i -> outDir </> ".cache" </> (itemPath i ++ ".partial.json")) items
  forM_ (zip items partials) $ \(item, _) -> do
    genPartialSearch env (itemPath item ++ ".partial.json") item

  fullOutPath %> \o -> do
    need partials
    mergePartialIndexes partials o

genPartialSearch :: SaraEnv -> FilePath -> Item 'Validated -> Rules ()
genPartialSearch env outPath item = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> ".cache" </> outPath
  
  fullOutPath %> \o -> do
    need [itemPath item]
    let (entry, content) = mkSearchEntry item
    generatePartialIndex entry content o

genSitemap :: SaraEnv -> FilePath -> [Item 'Validated] -> Rules ()
genSitemap env outPath items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert fullOutPath sg, ())
  
  fullOutPath %> \o -> do
    let sources = map itemPath items
    need sources
    generateSitemap (cfgSiteUrl (envConfig env)) items o

genRSS :: SaraEnv -> FilePath -> FeedConfig -> [Item 'Validated] -> Rules ()
genRSS env outPath cfg items = do
  let outDir = cfgOutputDirectory (envConfig env)
  let fullOutPath = outDir </> outPath
  liftIO $ atomicModifyIORef' (envSiteGraph env) $ \sg -> (HS.insert fullOutPath sg, ())
  
  fullOutPath %> \o -> do
    let sources = map itemPath items
    need sources
    generateRSS cfg items o

genGlobal :: SaraEnv -> SaraM () -> Rules ()
genGlobal env globalAction = do
  liftIO (runExceptT $ runWriterT $ runReaderT (unSaraM globalAction) env) >>= \case
    Left errs -> liftIO $ mapM_ (TIO.putStrLn . renderAnyErrorColor) errs
    Right ((), nestedDecls) -> do
      let nestedOutputs = collectOutputs env nestedDecls
      want nestedOutputs
      mapM_ (translateDecl env) nestedDecls
