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

deriving instance Show RoutingError
