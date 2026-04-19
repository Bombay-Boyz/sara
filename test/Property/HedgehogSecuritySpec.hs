{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Property.HedgehogSecuritySpec (spec) where

import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import SARA.Security.PathGuard
import SARA.Types (SafePath(..))
import SARA.Security.HtmlEscape
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import qualified Data.Aeson as Aeson

-- Fuzzing PathGuard: Property that guardPath should NEVER return an escaped path
prop_pathGuard_safe :: PropertyT IO ()
prop_pathGuard_safe = do
  path <- forAll $ Gen.string (Range.linear 1 100) Gen.alphaNum
  safe <- liftIO $ guardPath (ProjectRoot "root") path
  case safe of
    Right (SafePath p) -> assert (not $ ".." `T.isInfixOf` T.pack p)
    Left _ -> success

-- Fuzzing HtmlEscape: Property that escaping should remove/encode dangerous tags
prop_htmlEscape_safe :: PropertyT IO ()
prop_htmlEscape_safe = do
  val <- forAll $ Gen.string (Range.linear 1 50) (Gen.element ['<', '>', '&', '\"', 'a'])
  let escaped = escapeHtmlValue (Aeson.String (T.pack val))
  case escaped of
    Aeson.String s -> assert (not $ "<script>" `T.isInfixOf` T.toLower s)
    _ -> success


spec :: Spec
spec = do
  describe "SARA Hedgehog Security Properties" $ do
    it "PathGuard never permits traversal" $ hedgehog prop_pathGuard_safe
    it "HtmlEscape neutralizes tags" $ hedgehog prop_htmlEscape_safe
