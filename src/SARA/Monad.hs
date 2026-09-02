{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module SARA.Monad
  ( SaraM(..)
  , SaraEnv(..)
  , RuleDecl(..)
  , SiteGraph
  , BuildIssue(..)
  ) where

import Control.Monad.Reader (ReaderT, MonadReader)
import Control.Monad.Writer (WriterT, MonadWriter)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import Data.IORef (IORef)
import Data.HashSet (HashSet)
import qualified Data.Map.Strict as Map
import SARA.Config (SaraConfig, ProjectRoot)
import SARA.Error (AnySaraError)
import SARA.Types (GlobPattern, Item, ValidationState(..), FeedConfig)
import SARA.Internal.FrontmatterCache (FrontmatterCache)

-- | The site graph tracks all resolved output paths.
type SiteGraph = HashSet FilePath

-- | The SARA monad stack for rule declaration.
newtype SaraM a = SaraM
  { unSaraM :: ReaderT SaraEnv (WriterT [RuleDecl] (ExceptT [AnySaraError] IO)) a
  } deriving (Functor, Applicative, Monad, MonadIO, MonadReader SaraEnv, MonadWriter [RuleDecl], MonadError [AnySaraError])

data SaraEnv = SaraEnv
  { envConfig     :: !SaraConfig
  , envRoot       :: !ProjectRoot
  -- | The full set of output paths this build will produce, known
  --   ahead of time as a plain value — not an 'IORef' accumulated
  --   during Shake execution. See 'SARA.saraWithOptions' for where
  --   it's computed (once, via 'SARA.Internal.Planner.expandRules'
  --   and 'SARA.Internal.Planner.collectOutputs', before Shake ever
  --   runs) and 'SARA.Internal.Planner.checkInternalLinks' for the one
  --   place downstream that reads it. Making this a value rather than
  --   a mutable cell is the same "reify the plan as data, not as a
  --   side effect" discipline the rest of this module's 'RuleDecl'
  --   design already follows — it existed as an 'IORef' only because
  --   an earlier version of the planner filled it in incrementally as
  --   Shake rules were registered, which turned out to always finish
  --   with exactly the value 'collectOutputs' already computes
  --   directly from the (expanded) 'RuleDecl' list.
  , envSiteGraph  :: !SiteGraph
  , envRemapRules :: ![(Text, Text)]
  -- | Cache-busting query parameter for every discovered CSS\/JS
  --   asset, keyed by its site-relative URL (e.g. @\"\/assets\/style.css\"@
  --   -> a short content-hash string). Known ahead of time as a plain
  --   value, the same way and for the same reason as 'envSiteGraph' —
  --   computed once in 'SARA.saraWithOptions' from the expanded
  --   'RuleDecl' list's 'SARA.Monad.RuleDiscover' patterns, before
  --   Shake runs, and consulted by
  --   'SARA.Internal.Planner.rewriteAssetReferences' when writing out
  --   each page's final HTML. See that function's Haddock for why
  --   this is a query-parameter suffix rather than a renamed output
  --   file (engineering roadmap item #6).
  , envAssetManifest :: !(Map.Map Text Text)
  -- | Every SEO/link issue found while rendering, attributed to the
  --   file it came from. Mutable of necessity — unlike 'envSiteGraph',
  --   this genuinely cannot be known before rendering happens, since
  --   it depends on the actual rendered HTML, and Shake may render
  --   pages concurrently — but the *shape* stored here matters: this
  --   holds the same structured, per-file 'AnySaraError' values every
  --   check already produces, not a lossy summary 'Bool' of them.
  --   Accumulated via 'Data.IORef.atomicModifyIORef'', which is safe
  --   under Shake's concurrent scheduler.
  , envBuildIssues :: !(IORef [BuildIssue])
  -- | A persistent, on-disk cache of parsed frontmatter, closing
  --   engineering roadmap item #3: without it, 'SARA.DSL.match'
  --   re-reads and re-parses every matched file's frontmatter on
  --   every single rebuild, even for the files that weren't the one
  --   just saved. Mutable of necessity, like 'envBuildIssues' above —
  --   unlike 'envSiteGraph', this genuinely can't be reified as a
  --   plain up-front value, since its final contents depend on which
  --   files individually turn out to be unchanged vs. changed, known
  --   only incrementally as 'SARA.DSL.match' processes each one in
  --   turn. See 'SARA.Internal.FrontmatterCache' for the full design
  --   rationale, including why this has to be a standalone cache
  --   rather than a Shake oracle.
  , envFrontmatterCache :: !FrontmatterCache
  }

-- | Declarations produced by the DSL.
data RuleDecl
  = RuleDiscover !GlobPattern
  | RuleRender   !FilePath !(Item 'Validated) !FilePath
  | RuleRenderRaw !Text !(Item 'Validated) !FilePath
  | RuleRemap    ![(Text, Text)]
  | RuleSearch   !FilePath ![Item 'Validated]
  | RulePartialSearch !FilePath !(Item 'Validated)
  | RuleSitemap  !FilePath ![Item 'Validated]
  | RuleRSS      !FilePath !FeedConfig ![Item 'Validated]
  | RuleGlobal   !(SaraM ())

-- | A single, file-attributed problem found while rendering a page —
--   an SEO audit finding or a broken internal link. Carries enough to
--   build a report grouped by file, rather than the single
--   'Bool' ("something, somewhere, failed") this replaced. See
--   'SARA.saraWithOptions' for where the collected list becomes the
--   end-of-build report.
data BuildIssue = BuildIssue
  { biFile  :: !FilePath
  , biIssue :: !AnySaraError
  }
