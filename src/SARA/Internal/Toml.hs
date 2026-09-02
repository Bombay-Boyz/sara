{-# LANGUAGE OverloadedStrings #-}

-- | A small, self-contained TOML reader that parses directly to an
--   'Aeson.Object', built on 'megaparsec'.
--
--   __Why this exists__: the codebase's original TOML support went
--   through the @toml-parser@ package (Hackage). This build environment
--   has no route to Hackage — only a fixed set of non-Haskell package
--   registries and Ubuntu's apt archive are reachable — and @toml-parser@
--   is not packaged for Ubuntu. Every other dependency in this project
--   /is/ satisfied from apt; this module exists only to close that one
--   gap, using a dependency ('megaparsec') that already ships with the
--   rest of the toolchain.
--
--   __Scope__: TOML frontmatter in practice (Hugo's @+++@ blocks, and
--   any hand-written equivalent) is a shallow, flat-ish document: string,
--   numeric, boolean, date, and array values under bare or dotted keys,
--   occasionally grouped under @[table]@ headers. This parser covers
--   exactly that surface — basic and literal strings, integers, floats,
--   booleans, RFC 3339 dates\/datetimes (kept as their original text,
--   matching how the codebase's own 'valueToAeson' already treated
--   them before this change), arrays, inline tables, dotted keys, and
--   @[table]@\/@[[array-of-tables]]@ headers. It is intentionally not a
--   complete TOML 1.0 implementation (no multi-line strings, no
--   underscore-grouped or non-decimal integer literals) — those forms
--   are vanishingly rare in blog frontmatter, and the parser fails
--   closed (a clear parse error) rather than silently misreading them.
module SARA.Internal.Toml
  ( parseTomlToAeson
  ) where

import Data.Aeson (Object, Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Char (isAlphaNum)

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import qualified Data.Vector as V
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

-- | A fully-resolved dotted key path paired with the leaf value it
--   names, after 'toml' has already folded @[header]@ scoping into
--   the path. 'buildObject' only ever sees these, so it can stay a
--   simple, header-agnostic path-insertion fold.
data Entry = Entry [Text] Value

-- | Parse a full TOML document into a flat 'Aeson.Object', merging all
--   dotted-key and @[table]@ paths into the appropriately nested
--   structure.
parseTomlToAeson :: Text -> Either String Object
parseTomlToAeson input =
  case runParser (toml <* eof) "" (input <> "\n") of
    Left err  -> Left (errorBundlePretty err)
    Right obj -> Right obj

-- | A single top-level line: either a @[table]@ header (which changes
--   the prefix every following bare @key = value@ line nests under)
--   or a key\/value pair (nested under whatever the current prefix is).
data Line
  = HeaderLine [Text]
  | PairLine [Text] Value

-- | Fold top-level lines left to right, threading the "current table"
--   prefix set by the most recent @[header]@ through subsequent
--   key\/value lines — matching TOML's actual scoping rule, which
--   'toml''s earlier line-independent version did not.
toml :: Parser Object
toml = do
  sc
  ls <- many (choice [headerLine, pairLine] <* sc)
  let entries = go [] ls
  pure (buildObject entries)
  where
    go _ [] = []
    go _ (HeaderLine p : rest) = Entry p (Object KM.empty) : go p rest
    go prefix (PairLine p v : rest) = Entry (prefix <> p) v : go prefix rest

headerLine :: Parser Line
headerLine = HeaderLine <$> try (arrayHeaderPath <|> tableHeaderPath)

pairLine :: Parser Line
pairLine = do
  path <- dottedKeyPath
  _ <- lexeme (char '=')
  v <- value
  pure (PairLine path v)

tableHeaderPath :: Parser [Text]
tableHeaderPath = between (char '[') (char ']') dottedKeyPath

arrayHeaderPath :: Parser [Text]
arrayHeaderPath = between (string "[[") (string "]]") dottedKeyPath

-- | Fold a flat entry list into nested 'Aeson.Object's, right-associating
--   each path so @a.b.c = 1@ and a later @[a] \n b.c = 1@ land in the
--   same shape. Later entries win on key collision, matching TOML's
--   "last write, same key" semantics closely enough for frontmatter.
buildObject :: [Entry] -> Object
buildObject = foldl' insertEntry KM.empty
  where
    insertEntry obj (Entry path v) = insertPath path v obj

    insertPath [] _ obj = obj
    insertPath [k] v obj = KM.insert (K.fromText k) v obj
    insertPath (k:ks) v obj =
      let nested = case KM.lookup (K.fromText k) obj of
            Just (Object o) -> o
            _               -> KM.empty
      in KM.insert (K.fromText k) (Object (insertPath ks v nested)) obj

--------------------------------------------------------------------------------
-- Lexing

sc :: Parser ()
sc = L.space space1 lineComment empty
  where lineComment = L.skipLineComment "#"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

dottedKeyPath :: Parser [Text]
dottedKeyPath = lexeme (tomlKey `sepBy1` char '.')

tomlKey :: Parser Text
tomlKey = sc *> (bareKey <|> basicStringRaw <|> literalStringRaw) <* sc
  where
    bareKey = T.pack <$> some (satisfy (\c -> isAlphaNum c || c == '_' || c == '-')) <?> "key"

--------------------------------------------------------------------------------
-- Values

value :: Parser Value
value =
  lexeme $
    choice
      [ inlineTable
      , arrayValue
      , Bool True  <$ string "true"
      , Bool False <$ string "false"
      , stringValue
      , dateTimeOrNumber
      ]

stringValue :: Parser Value
stringValue = String <$> (basicStringRaw <|> literalStringRaw)

-- | A basic (double-quoted) string, with the small set of escapes
--   frontmatter values actually use.
basicStringRaw :: Parser Text
basicStringRaw = char '"' *> (T.pack <$> manyTill charLit (char '"'))
  where
    charLit = escaped <|> anySingleBut '"'
    escaped = char '\\' *> escapedChar
    escapedChar =
      choice
        [ '"'  <$ char '"'
        , '\\' <$ char '\\'
        , '\n' <$ char 'n'
        , '\t' <$ char 't'
        , '\r' <$ char 'r'
        ]

-- | A literal (single-quoted) string: no escapes at all, verbatim.
literalStringRaw :: Parser Text
literalStringRaw = char '\'' *> (T.pack <$> manyTill anySingle (char '\''))

-- | RFC 3339 dates/datetimes are kept as their original source text —
--   matching how the codebase's prior 'toml-parser'-based
--   'valueToAeson' rendered them (@T.pack . show@ on the parsed
--   time value) — rather than parsed into a 'UTCTime' and
--   re-rendered, since frontmatter only ever round-trips these
--   through 'Aeson.String' on the way to a template or JSON-LD
--   context, never as a computed time value in Haskell.
dateTimeOrNumber :: Parser Value
dateTimeOrNumber = try dateTimeLit <|> numberLit

dateTimeLit :: Parser Value
dateTimeLit = do
  y <- count 4 digitChar
  _ <- char '-'
  m <- count 2 digitChar
  _ <- char '-'
  d <- count 2 digitChar
  rest <- option "" timeSuffix
  pure (String (T.pack (y <> "-" <> m <> "-" <> d <> rest)))
  where
    timeSuffix = do
      _ <- char 'T' <|> char 't' <|> char ' '
      hh <- count 2 digitChar
      _ <- char ':'
      mm <- count 2 digitChar
      _ <- char ':'
      ss <- count 2 digitChar
      frac <- option "" ((:) <$> char '.' <*> some digitChar)
      tz <- option "" (T.unpack <$> (string "Z" <|> zoneOffset))
      pure ('T' : hh <> ":" <> mm <> ":" <> ss <> frac <> tz)
    zoneOffset = do
      sign <- string "+" <|> string "-"
      hh <- T.pack <$> count 2 digitChar
      _ <- char ':'
      mm <- T.pack <$> count 2 digitChar
      pure (sign <> hh <> ":" <> mm)

numberLit :: Parser Value
numberLit = do
  sign <- option "" (string "-" <|> string "+")
  intPart <- some digitChar
  fracPart <- option "" ((:) <$> char '.' <*> some digitChar)
  expPart <- option "" expPartP
  let n = read (T.unpack sign <> intPart <> fracPart <> expPart) :: Double
  pure (Number (realToFrac n))
  where
    expPartP = do
      e <- char 'e' <|> char 'E'
      s <- option "" (string "+" <|> string "-")
      ds <- some digitChar
      pure (e : T.unpack s <> ds)

arrayValue :: Parser Value
arrayValue = do
  _ <- symbol "["
  vs <- value `sepEndBy` symbol ","
  _ <- symbol "]"
  pure (Array (V.fromList vs))

inlineTable :: Parser Value
inlineTable = do
  _ <- symbol "{"
  pairs <- inlinePair `sepBy` symbol ","
  _ <- symbol "}"
  pure (Object (buildObject [Entry p v | (p, v) <- pairs]))
  where
    inlinePair = do
      path <- dottedKeyPath
      _ <- symbol "="
      v <- value
      pure (path, v)
