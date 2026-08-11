{-# OPTIONS_GHC -Wno-partial-fields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
module SARA.Routing.Error
  ( RoutingError(..)
  ) where

import Data.Text (Text)

data RoutingError where
  RouteRegexInvalid
    :: { rtPattern :: !Text, rtDetail :: !Text }
    -> RoutingError
  RouteConflict
    :: { rtFile1 :: !FilePath, rtFile2 :: !FilePath, rtOutput :: !FilePath }
    -> RoutingError
  -- | A resolved output path contains a character or reserved base
  --   name forbidden on Windows (\<\>:"\|?* and CON\/PRN\/AUX\/NUL\/COM1-9\/LPT1-9).
  --   Hugo's own docs candidly note it does *not* sanitise
  --   user-supplied 'url' front matter for this, so a path perfectly
  --   valid on the author's Linux/macOS machine can fail outright for
  --   a Windows reader of the same repository, or a Windows CI runner.
  --   Since a route's output path can come from a regex replacement
  --   over arbitrary captured text — not just the literal source
  --   filename — this can't be ruled out by construction the way
  --   'SARA.Security.PathGuard' rules out traversal; it's checked here
  --   once the path is known, before it's ever written to.
  RouteUnsafeForWindows
    :: { rtPath :: !FilePath, rtReason :: !Text }
    -> RoutingError

deriving instance Show RoutingError
