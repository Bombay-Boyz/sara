{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module SARA.Routing.Engine
  ( resolveRoute
  , detectRouteConflicts
  , regexRoute
  ) where

import SARA.Types (Route(..), RouteState(..), SafeRegex(..))
import SARA.Security.RegexGuard (mkSafeRegex, unSafeRegex)
import SARA.Error (SaraError(..), SaraErrorKind(..))
import qualified SARA.Routing.Error as RE
import System.FilePath ((</>), replaceExtension, splitFileName, dropExtension)
import Data.List (groupBy, sortOn)
import qualified Data.List as L
import Data.Function (on)
import qualified Text.Regex.PCRE.Text as RE
import Data.Array ((!), bounds)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void (Void)
import Control.Monad (void)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)

type Parser = Parsec Void T.Text

-- | Smart constructor for regex routes.
regexRoute :: T.Text -> T.Text -> Either (SaraError 'EKSecurity) (Route 'Abstract)
regexRoute pat repl = case mkSafeRegex pat of
  Right safe -> Right $ RegexRoute safe repl
  Left err -> Left err

-- | Apply an abstract route to a concrete source path.
--   Controlled purity: Regex execution is performant but requires IO internally.
resolveRoute
  :: Route 'Abstract
  -> FilePath          -- ^ Source path
  -> Either RE.RoutingError (Route 'Resolved)
resolveRoute route_ sourcePath = case route_ of
  SlugRoute ->
    Right $ ResolvedRoute (replaceExtension sourcePath "html")
  PrettyRoute ->
    let (dir, file) = splitFileName sourcePath
        name = dropExtension file
    in Right $ ResolvedRoute (dir </> name </> "index.html")
  LiteralRoute path ->
    Right $ ResolvedRoute path
  RegexRoute { rrSafeRegex = safeRegex, rrReplacement = repl } ->
    let pat = unSafeRegex safeRegex
        pathText = T.pack sourcePath
        compiled = unsafePerformIO $ RE.compile RE.compBlank RE.execBlank pat
    in case compiled of
         Left (_, err) -> Left $ RE.RouteRegexInvalid (T.pack sourcePath) (T.pack err)
         Right compiledRegex ->
             let matches = unsafePerformIO $ RE.execute compiledRegex pathText
             in case matches of
                  Right (Just arr) -> 
                    let captures = map (\i -> let (off, len) = arr ! i
                                              in T.take len (T.drop off pathText)) 
                                       [0 .. snd (bounds arr)]
                    in Right $ ResolvedRoute (T.unpack $ interpolateCaptures captures repl)
                  _ -> 
                    Right $ ResolvedRoute (replaceExtension sourcePath "html")

-- | Replaces \0, \1, \2... in the replacement string with the corresponding capture group.
interpolateCaptures :: [T.Text] -> T.Text -> T.Text
interpolateCaptures caps repl =
  case parse (pInterpolate caps) "" repl of
    Left _ -> repl 
    Right res -> res

pInterpolate :: [T.Text] -> Parser T.Text
pInterpolate caps = T.concat <$> many (pCapture caps <|> pLiteral)

pCapture :: [T.Text] -> Parser T.Text
pCapture caps = do
  void (chunk "\\")
  digit <- digitChar
  let idx = read [digit] :: Int
  return $ case drop idx caps of
    (val:_) -> val
    []      -> "\\" <> T.singleton digit

pLiteral :: Parser T.Text
pLiteral = (T.singleton <$> anySingleBut '\\') <|> (chunk "\\\\" >> return "\\")

-- | Detect route conflicts in a list of resolved routes.
detectRouteConflicts
  :: [(FilePath, Route 'Resolved)]
  -> [RE.RoutingError]
detectRouteConflicts routes =
  let groups = groupBy ((==) `on` (getResolvedPath . snd)) (sortOn (getResolvedPath . snd) routes)
  in concatMap checkGroup groups
  where
    getResolvedPath :: Route 'Resolved -> FilePath
    getResolvedPath (ResolvedRoute p) = p
    
    checkGroup :: [(FilePath, Route 'Resolved)] -> [RE.RoutingError]
    checkGroup [] = []
    checkGroup [_] = []
    checkGroup ((f1, r1):rest) = 
      let (f2, _) = L.last rest
      in [RE.RouteConflict f1 f2 (getResolvedPath r1)]
