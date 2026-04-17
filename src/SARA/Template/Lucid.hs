{-# LANGUAGE OverloadedStrings #-}

module SARA.Template.Lucid
  ( renderLucid
  ) where

import Lucid
import SARA.Types (Item(..))
import Data.Text (Text)
import qualified Data.Text.Lazy as TL

-- | Renders an Item using a Lucid template.
renderLucid :: Html () -> Item v -> Text
renderLucid template _ = TL.toStrict $ renderText template
