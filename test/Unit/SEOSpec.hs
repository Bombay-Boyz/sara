{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Unit.SEOSpec (spec) where

import Test.Hspec
import SARA.SEO.Audit

spec :: Spec
spec = do
  describe "SEO Auditing" $ do
    it "detects missing alt text" $ do
      let html = "<html><body><img src=\"test.png\"></body></html>"
      let res = auditRenderedHTML "test.html" html
      case res of
        AuditIssues _ _ -> success
        _ -> expectationFailure "Should have found SEO issues"

    it "passes valid HTML" $ do
      let html = "<html><head><title>Title</title><meta name=\"description\" content=\"A very detailed description of this wonderful site that meets length requirements.\"><meta property=\"og:title\" content=\"Title\"><meta property=\"og:image\" content=\"img.png\"></head><body><h1>Main Title</h1><img src=\"test.png\" alt=\"text\"></body></html>"
      let res = auditRenderedHTML "test.html" html
      res `shouldBe` AuditPassed

success :: Expectation
success = return ()
