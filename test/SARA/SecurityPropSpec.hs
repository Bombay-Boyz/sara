module SARA.SecurityPropSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Monadic (monadicIO, run, assert)
import SARA.Security.PathGuard
import System.FilePath (splitDirectories)
import qualified Data.List as L

spec :: Spec
spec = do
  describe "Security Properties" $ do
    prop "guardPath never escapes root" $ \candidatePath ->
      not (null candidatePath) ==> monadicIO $ do
        let root = ProjectRoot "/tmp/sara-root"
        result <- run (guardPath root candidatePath)
        case result of
          Left _ -> assert True
          Right safePath ->
            let rootSegments = splitDirectories "/tmp/sara-root"
                candSegments = splitDirectories (unSafePath safePath)
            in assert (rootSegments `L.isPrefixOf` candSegments)
