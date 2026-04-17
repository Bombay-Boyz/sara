{-# LANGUAGE OverloadedStrings #-}

module SARA.Markdown.Shortcode
  ( Shortcode(..)
  , parseShortcodes
  , expandShortcodes
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Control.Monad (void)

type Parser = Parsec Void Text

data Shortcode = Shortcode
  { scName :: !Text
  , scArgs :: !(Map Text Text)
  } deriving (Show, Eq)

-- | Parser for {{% name key="value" %}}
pShortcode :: Parser Shortcode
pShortcode = do
  void (string "{{% ")
  name <- T.pack <$> some alphaNumChar
  void space1
  args <- pArgs
  void (string "%}}")
  return $ Shortcode name args

pArgs :: Parser (Map Text Text)
pArgs = Map.fromList <$> many (pPair <* space)
  where
    pPair = do
      key <- T.pack <$> some alphaNumChar
      void (char '=')
      val <- char '"' *> (T.pack <$> manyTill L.charLiteral (char '"'))
      return (key, val)

parseShortcodes :: Text -> [Shortcode]
parseShortcodes t = case parse (many (try pShortcode <|> (anySingle >> return (Shortcode "ignore" Map.empty)))) "" t of
  Left _ -> []
  Right scs -> filter (\s -> scName s /= "ignore") scs

-- | Expands shortcodes using a registry of handlers.
expandShortcodes :: (Shortcode -> Text) -> Text -> Text
expandShortcodes handler t =
  case parse (pBody handler) "" t of
    Left _ -> t
    Right res -> res

pBody :: (Shortcode -> Text) -> Parser Text
pBody handler = T.concat <$> many (try (handler <$> pShortcode) <|> (T.singleton <$> anySingle))
