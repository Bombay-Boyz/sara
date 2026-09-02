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
import Control.Concurrent (getNumCapabilities)
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
  -- Page rendering, image processing, and asset copying are almost
  -- entirely independent per-file '%>' rules — an embarrassingly
  -- parallel workload that previously ran on a single core, since
  -- 'shakeThreads' defaults to 1 when unset.
  --
  -- 'shakeThreads = 0' is Shake's own documented value for "use all
  -- available capabilities", but empirically (verified directly
  -- against this project's installed shake-0.19.7, via a minimal
  -- reproduction with a known, measurable expected speedup) it did
  -- *not* enable parallel execution the way the documentation
  -- describes, while an explicit positive thread count did, exactly
  -- matching the expected speedup. Querying 'getNumCapabilities'
  -- directly and passing that explicit count sidesteps whatever gap
  -- exists between the documented and observed behavior of 0, while
  -- still achieving the same "use all available cores" goal —
  -- capped at a sane minimum of 1 for the pathological case of a
  -- runtime somehow reporting zero capabilities.
  --
  -- This number is only meaningful with the threaded RTS active
  -- (GHC's non-threaded runtime has exactly one capability no matter
  -- what 'shakeThreads' requests) — see @sara.cabal@'s @ghc-options@
  -- for the @-threaded -rtsopts "-with-rtsopts=-N"@ this depends on.
  --
  -- Verified safe to enable at all before making this change: every
  -- cross-file dependency in this rule graph (search-index partials,
  -- per-item source files, templates) goes through an explicit
  -- 'need'/'needBlake3' — the mechanism Shake itself uses to
  -- correctly serialize exactly where required while still running
  -- independent targets concurrently — rather than any rule relying
  -- on incidental ordering or shared mutable state outside Shake's
  -- own tracking.
  caps <- max 1 <$> getNumCapabilities
  let options = (shakeOptions 
        { shakeFiles = root </> "_build"
        , shakeVerbosity = Quiet
        , shakeThreads = caps
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
