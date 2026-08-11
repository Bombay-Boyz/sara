{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeApplications #-}

-- | Tests for the typed-content-metadata feature added when 'Item' was
--   generalised into @'ItemP' v meta@ (with @type Item v = ItemP v
--   Aeson.Object@ preserving every existing untyped call site).
--
--   Three properties matter here, and each gets its own test rather
--   than one broad "it works" case, per 5.4\/5.5 of the Haskell
--   Engineering Standard (every stated guarantee needs its own,
--   separately-checkable test):
--
--     1. A well-formed post decodes into the caller's own record, with
--        real, compiler-checked field access — not a 'KM.lookup'.
--     2. A malformed post (missing or wrong-shaped field) is rejected
--        at 'readMarkdownAs' with a named 'SaraError', not a runtime
--        surprise three functions later.
--     3. 'toRenderableItem' is a faithful round-trip back to the
--        untyped 'Item' the rest of the pipeline (Mustache, RSS,
--        sitemap, JSON-LD) already knows how to handle.
module SARA.TypedMetadataSpec (spec) where

import Test.Hspec
import SARA
import SARA.Monad (SaraM(..), SaraEnv(..), RuleDecl)
import GHC.Generics (Generic)
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import System.IO.Temp (withSystemTempDirectory)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import qualified Data.Text.IO as TIO
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriterT)
import Control.Monad.Except (runExceptT)
import Data.IORef (newIORef)
import qualified Data.HashSet as HS

-- | A minimal, realistic typed schema: exactly the kind of record a
--   real blog would define instead of reaching for 'KM.lookup'
--   everywhere. The 'FromJSON'/'ToJSON' instances strip the "bp"
--   prefix so frontmatter can use plain "title"/"tags" keys, the
--   conventional shape for this and every other Haskell field-prefix
--   naming style — Aeson's derived 'Generic' instance does not do this
--   for you by default, which is exactly the kind of frontmatter/schema
--   mismatch 'readMarkdownAs' is meant to catch at decode time instead
--   of it surfacing later as a wrong or missing value.
data BlogPost = BlogPost
  { bpTitle :: Text
  , bpTags  :: [Text]
  } deriving (Show, Eq, Generic)

blogPostJSONOptions :: Aeson.Options
blogPostJSONOptions = Aeson.defaultOptions { Aeson.fieldLabelModifier = Aeson.camelTo2 '_' . drop 2 }

instance Aeson.FromJSON BlogPost where
  parseJSON = Aeson.genericParseJSON blogPostJSONOptions

instance Aeson.ToJSON BlogPost where
  toJSON = Aeson.genericToJSON blogPostJSONOptions

-- | Run a 'SaraM' action against a fresh, minimal environment rooted at
--   a temp directory — enough machinery for 'readMarkdownAs' (which
--   only needs 'envRoot' and 'envRemapRules') without pulling in a full
--   'saraWithOptions' build.
runInTempProject :: FilePath -> SaraM a -> IO (Either [AnySaraError] (a, [RuleDecl]))
runInTempProject tmpDir action = do
  errorRef <- newIORef []
  root <- mkProjectRoot tmpDir
  let env = SaraEnv
        { envConfig = defaultConfig
        , envRoot = root
        , envSiteGraph = HS.empty
        , envRemapRules = []
        , envBuildIssues = errorRef
        }
  runExceptT $ runWriterT $ runReaderT (unSaraM action) env

spec :: Spec
spec = describe "Typed content metadata (ItemP / readMarkdownAs / toRenderableItem)" $ do

  it "decodes well-formed frontmatter into the caller's own record, with real field access" $
    withSystemTempDirectory "sara-typed-meta" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "posts")
      TIO.writeFile (tmpDir </> "posts" </> "hello.md") $
        "---\ntitle: Hello Typed World\ntags:\n  - haskell\n  - ssg\n---\nBody text."
      result <- runInTempProject tmpDir (readMarkdownAs @BlogPost (tmpDir </> "posts" </> "hello.md"))
      case result of
        Right (item, _) -> do
          bpTitle (itemMeta item) `shouldBe` "Hello Typed World"
          bpTags (itemMeta item) `shouldBe` ["haskell", "ssg"]
        Left errs -> expectationFailure $ "Expected a well-typed decode, got: " ++ show errs

  it "rejects frontmatter missing a required field, as a named SaraError, not a runtime crash" $
    withSystemTempDirectory "sara-typed-meta-missing" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "posts")
      -- No "tags" key at all — BlogPost has no Maybe/default for it,
      -- so this must fail to decode, not silently produce [] or bottom.
      TIO.writeFile (tmpDir </> "posts" </> "bad.md") $
        "---\ntitle: Missing Tags\n---\nBody text."
      result <- runInTempProject tmpDir (readMarkdownAs @BlogPost (tmpDir </> "posts" </> "bad.md"))
      case result of
        Left _ -> pure () -- correctly rejected
        Right (item, _) -> expectationFailure $
          "Expected decoding to fail for missing 'tags', got a value with title: " ++ show (bpTitle (itemMeta item))

  it "toRenderableItem round-trips a typed item's metadata back into the untyped Aeson.Object form" $
    withSystemTempDirectory "sara-typed-meta-roundtrip" $ \tmpDir -> do
      createDirectoryIfMissing True (tmpDir </> "posts")
      TIO.writeFile (tmpDir </> "posts" </> "hello.md") $
        "---\ntitle: Round Trip\ntags:\n  - a\n  - b\n---\nBody."
      result <- runInTempProject tmpDir (readMarkdownAs @BlogPost (tmpDir </> "posts" </> "hello.md"))
      case result of
        Left errs -> expectationFailure $ "Expected typed decode to succeed, got: " ++ show errs
        Right (typedItem, _) -> do
          let untyped = toRenderableItem typedItem
          KM.lookup "title" (itemMeta untyped) `shouldBe` Just (Aeson.String "Round Trip")
          KM.lookup "tags" (itemMeta untyped) `shouldBe` Just (Aeson.toJSON (["a", "b"] :: [Text]))
          -- Every field besides itemMeta's type is untouched by the conversion.
          itemPath untyped `shouldBe` itemPath typedItem
          itemBody untyped `shouldBe` itemBody typedItem
