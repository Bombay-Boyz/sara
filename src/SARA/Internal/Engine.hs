{-# LANGUAGE OverloadedStrings #-}

module SARA.Internal.Engine
  ( runBuild
  ) where

import Development.Shake
import SARA.Monad (RuleDecl, SaraEnv(..))
import SARA.Config (SaraConfig(..))
import SARA.Internal.Planner (planRules)
import SARA.Internal.Hash (addBlake3Oracle, addLQIPOracle)
import SARA.Template.Renderer (addTemplateOracle)
import SARA.Security.PathGuard (ProjectRoot(..))
import System.FilePath ((</>))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import System.Environment (withArgs)
import Control.Monad (forM_)
import qualified Data.HashSet as HS

-- | Executes the SARA build engine using Shake — or, if
--   'cfgDryRun' is set, reports the build plan without performing any
--   of it: no Shake database is opened, no oracle runs, and nothing on
--   disk is read or written. This is the one place '--dry-run' (parsed
--   in "Main" and threaded through as 'cfgDryRun') actually takes
--   effect; per 1.11 of the Haskell Engineering Standard, a flag that
--   reaches here and is silently ignored is worse than the flag not
--   existing, so this branch is what makes '--dry-run' a real,
--   observable difference in behaviour rather than a parsed-and-dropped
--   value.
runBuild :: SaraEnv -> [RuleDecl] -> IO ()
runBuild env rules
  | cfgDryRun (envConfig env) = previewBuild env
  | otherwise                 = executeBuild env rules

-- | Print every output path this build will produce, without touching
--   the filesystem or running Shake. 'envSiteGraph' is already the
--   full, precomputed plan (see its Haddock in "SARA.Monad") — this
--   function does no computation of its own, only formatting.
previewBuild :: SaraEnv -> IO ()
previewBuild env = do
  let outputs = HS.toList (envSiteGraph env)
  putStrLn "SARA: Dry run — no files will be written. Planned outputs:"
  if null outputs
    then putStrLn "  (no outputs would be produced by the current rules)"
    else forM_ outputs $ \o -> putStrLn ("  " ++ o)
  putStrLn $ "SARA: Dry run complete. " ++ show (length outputs) ++ " file(s) would be written."

-- | The real build: runs the full Shake pipeline and writes output.
executeBuild :: SaraEnv -> [RuleDecl] -> IO ()
executeBuild env rules = do
  start <- getCPUTime
  
  let (ProjectRoot root) = envRoot env
  let options = (shakeOptions 
        { shakeFiles = root </> "_build"
        , shakeVerbosity = Quiet
        })
  
  -- Internal isolation: Shake never sees the CLI arguments
  withArgs [] $ shake options $ do
    addBlake3Oracle
    addLQIPOracle
    addTemplateOracle
    planRules env rules
  
  end <- getCPUTime
  let picosPerSec :: Integer
      picosPerSec = 10^(12 :: Integer)
  let diff :: Double
      diff = fromIntegral (end - start) / fromIntegral picosPerSec
  printf "Build completed in %0.3fs\n" diff
