{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}

module SARA.Routing.Engine
  ( resolveRoute
  , detectRouteConflicts
  , regexRoute
  ) where

import SARA.Types (Route(..), RouteState(..), SafeRegex(..), SPath)
import SARA.Error (SaraError(..), SaraErrorKind(..))
import SARA.Security.RegexGuard (compileSafeRegex)
import System.FilePath (replaceExtension, takeBaseName, takeDirectory, (</>))
import qualified Data.Text as T
import Data.Text (Text)
import qualified Text.Regex.TDFA as Regex
import Data.List (groupBy, sortOn)
import Data.Function (on)

-- | Compiles and validates a regex route.
regexRoute :: Text -> Text -> IO (Either (SaraError 'EKSecurity) (Route 'Abstract))
regexRoute pat repl = do
  res <- compileSafeRegex pat
  case res of
    Right safe -> return $ Right $ RegexRoute safe repl
    Left err -> return $ Left err

-- | Resolves an abstract route into a concrete SPath.
resolveRoute 
  :: Route 'Abstract 
  -> FilePath 
  -> IO (Either (SaraError 'EKRouting) (Route 'Resolved))
resolveRoute r sourcePath = case r of
  SlugRoute -> 
    return $ Right $ ResolvedRoute (T.pack $ replaceExtension sourcePath "html")
    
  PrettyRoute -> 
    let name = takeBaseName sourcePath
        dir = takeDirectory sourcePath
    in return $ Right $ ResolvedRoute (T.pack $ dir </> name </> "index.html")
    
  RegexRoute (SafeRegex pat) repl ->
    let (before, matched, after, captures) = (T.pack sourcePath) Regex.=~ pat :: (Text, Text, Text, [Text])
    in if not (T.null matched)
       then let resolved = interpolateCaptures captures repl
            in return $ Right $ ResolvedRoute resolved
       else -- Fallback if regex doesn't match
            return $ Right $ ResolvedRoute (T.pack $ replaceExtension sourcePath "html")

  LiteralRoute p -> return $ Right $ ResolvedRoute p

-- | Injects regex capture groups into replacement string ($1, $2...).
interpolateCaptures :: [Text] -> Text -> Text
interpolateCaptures captures repl = foldr replaceGroup repl (zip [1..] captures)
  where
    replaceGroup (i, cap) acc = T.replace ("$" <> T.pack (show i)) cap acc

-- | Checks for output path conflicts in a set of resolved routes.
detectRouteConflicts :: [(FilePath, Route 'Resolved)] -> [SaraError 'EKRouting]
detectRouteConflicts routesString =
  let routes = map (\(f, r) -> (T.pack f, r)) routesString
      groups = groupBy ((==) `on` (getResolvedPath . snd)) 
                       (sortOn (getResolvedPath . snd) routes)
  in concatMap checkGroup groups
  where
    getResolvedPath :: Route 'Resolved -> SPath
    getResolvedPath (ResolvedRoute p) = p

    checkGroup :: [(SPath, Route 'Resolved)] -> [SaraError 'EKRouting]
    checkGroup [] = []
    checkGroup [_] = []
    checkGroup ((f1, r1) : rest) = 
      let out = getResolvedPath r1
      in map (\(f2, _) -> RouteConflict f1 f2 (T.unpack out)) rest
