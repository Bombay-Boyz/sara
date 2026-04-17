{-# LANGUAGE OverloadedStrings #-}

module SARA.Internal.Engine
  ( runBuild
  ) where

import Development.Shake
import SARA.Monad (RuleDecl, SaraEnv(..))
import SARA.Internal.Planner (planRules)
import SARA.Internal.Hash (addBlake3Oracle, addLQIPOracle)
import SARA.Template.Renderer (addTemplateOracle)
import SARA.Security.PathGuard (ProjectRoot(..))
import System.FilePath ((</>))
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import System.Environment (withArgs)

-- | Executes the SARA build engine using Shake.
runBuild :: SaraEnv -> [RuleDecl] -> IO ()
runBuild env rules = do
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
