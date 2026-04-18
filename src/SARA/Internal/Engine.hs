{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module SARA.Internal.Engine
  ( runBuild
  ) where

import Development.Shake
import SARA.Monad (RuleDecl(..), SaraEnv(..), SaraState(..))
import SARA.Internal.Planner (planRules)
import SARA.Internal.Hash (addBlake3Oracle, addLQIPOracle, addDataOracle)
import SARA.Template.Renderer (addTemplateOracle)
import SARA.Security.PathGuard (ProjectRoot(..))
import System.FilePath ((</>))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import UnliftIO.IORef (atomicModifyIORef')
import UnliftIO.MVar (newMVar)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import System.Directory (canonicalizePath)
import Data.Maybe (fromMaybe)
import qualified Data.List as L

-- | Executes the SARA build engine using Shake.
runBuild :: SaraEnv -> [RuleDecl] -> IO ()
runBuild env rules = do
  start <- getCPUTime
  
  -- Pre-populate template cache MVars (Fix U-02)
  -- We collect ALL templates from ALL rules to ensure cache is ready.
  let templates = L.nub [ T.unpack t | RuleRender t _ _ <- rules ]
  absTemplates <- mapM (\t -> (t,) <$> canonicalizePath t) templates
  tplMap <- Map.fromList <$> mapM (\(_orig, abs) -> (T.pack abs,) <$> newMVar Nothing) absTemplates
  atomicModifyIORef' (envState env) $ \s ->
    (s { stateTemplateCache = Map.union tplMap (stateTemplateCache s) }, ())

  let (ProjectRoot root) = envRoot env
  let options = (shakeOptions 
        { shakeFiles = root </> "_build"
        , shakeVerbosity = Quiet
        , shakeThreads = fromMaybe 0 (envJobs env)
        })
  
  -- Industrial multicore scaling: shake respects our options
  shake options $ do
    addBlake3Oracle env
    addLQIPOracle env
    addDataOracle env
    addTemplateOracle env
    planRules env rules
  
  end <- getCPUTime
  let picosPerSec :: Integer
      picosPerSec = 10^(12 :: Integer)
  let diff :: Double
      diff = fromIntegral (end - start) / fromIntegral picosPerSec
  printf "Build completed in %0.3fs\n" diff
