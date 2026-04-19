module Property.SecurityPropSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import SARA.Security.PathGuard
import SARA.Types (SafePath(..))
import System.FilePath (splitDirectories)
import qualified Data.List as L

spec :: Spec
spec = do
  describe "Security Properties" $ do
    prop "guardPath never escapes root" $ \candidatePath ->
      (not (null candidatePath)) ==> ioProperty $ do
          let root = ProjectRoot "/tmp/sara-root"
          result <- guardPath root candidatePath
          return $ case result of
            Left _ -> property True
            Right (SafePath canPath) ->
              let rootSegments = splitDirectories "/tmp/sara-root"
                  candSegments = splitDirectories canPath
              in property $ rootSegments `L.isPrefixOf` candSegments
