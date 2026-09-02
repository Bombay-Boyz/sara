{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- | Compile-time-adjacent template field validation — engineering
--   roadmap item #4, and the single most defensible feature on that
--   whole list: no other mainstream static site generator (Hugo,
--   Zola, Jekyll, Eleventy, Hakyll) can catch a typo'd template field
--   reference as a build failure the way this does, because none of
--   them have a typed record sitting on the other end of the
--   template context the way 'SARA.DSL.readMarkdownAs' already gives
--   this codebase.
--
--   __What this actually checks__: every top-level @{{ field }}@,
--   @{{{ field }}}@, @{{# field }}@, and @{{^ field }}@ reference in a
--   template against the field names of the Haskell record type a
--   page's typed metadata was parsed into (via 'GHC.Generics'), plus
--   the handful of context keys 'SARA.Internal.Planner.genRender'
--   always injects regardless of metadata shape (@itemBody@,
--   @siteTitle@, @siteUrl@, @siteAuthor@). A reference to a field the
--   record doesn't have — a typo, a renamed field the template wasn't
--   updated for — fails the build with the exact file, line, and
--   field name, before ever reaching a user's browser as a silently
--   blank spot on the page.
--
--   __What this deliberately does not check__: dotted\/nested field
--   access (@{{ author.name }}@) and identifiers referenced from
--   inside a section body (@{{# tags }}{{.}}{{\/tags}}@, where @.@
--   refers to each list element, not a top-level field) are both
--   skipped outright, not validated against the wrong scope. Doing
--   either correctly would require walking the *type structure* of
--   nested fields (what fields does @author@ itself have?), not just
--   a flat top-level field list — a materially bigger feature this
--   deliberately stops short of, rather than validate against the
--   wrong context and produce confusing false positives\/negatives.
--   Every reference this module doesn't understand is left
--   unvalidated, never flagged as wrong.
--
--   __Why a hand-written scanner instead of the compiled Mustache
--   AST__: @mustache@'s own parsed-template representation
--   ('Text.Mustache.Internal.Types.Node' and friends) lives in a
--   module the package deliberately does not expose —
--   confirmed directly by trying to import it, which GHC rejects
--   with \"it is a hidden module\". This module's 'scanTemplateFields'
--   is a small, standalone, from-scratch scanner over the raw
--   template text instead, intentionally narrow in scope (see above)
--   rather than attempting to reimplement Mustache's full grammar.
module SARA.Internal.TemplateCheck
  ( GFieldNames(..)
  , fieldNamesOf
  , scanTemplateFields
  , alwaysAvailableContextKeys
  , unknownTemplateFields
  ) where

import Data.List (nub)
import Data.Proxy (Proxy(..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics

--------------------------------------------------------------------------------
-- Part 1: field names of a record type, via GHC.Generics (no
-- Template Haskell — this is the same style of technique Aeson's own
-- generic deriving uses internally).

class GFieldNames f where
  gFieldNames :: Proxy f -> [Text]

instance GFieldNames f => GFieldNames (D1 c f) where
  gFieldNames _ = gFieldNames (Proxy @f)

instance GFieldNames f => GFieldNames (C1 c f) where
  gFieldNames _ = gFieldNames (Proxy @f)

instance (GFieldNames a, GFieldNames b) => GFieldNames (a :*: b) where
  gFieldNames _ = gFieldNames (Proxy @a) ++ gFieldNames (Proxy @b)

-- | A record with no fields at all (e.g. derived for a non-record
--   constructor) contributes no names, rather than failing to
--   compile — 'renderTyped' still works for such a type, it just has
--   nothing but the always-available context keys to validate
--   against.
instance GFieldNames U1 where
  gFieldNames _ = []

instance Selector s => GFieldNames (S1 s (Rec0 a)) where
  gFieldNames _ = [T.pack (selName (undefined :: S1 s (Rec0 a) ()))]

-- | The field names of a record type, via its 'Generic' instance.
--   Used as @fieldNamesOf (Proxy \@MyMetadata)@. The explicit
--   'Generic' constraint here is logically necessary (there's no
--   other way 'Rep a' is well-defined at all), but newer GHC's
--   redundant-constraint checker flags it anyway, since resolving
--   'GFieldNames (Rep a)' alone already forces 'Generic a' into scope
--   transitively — so it really can be dropped from the signature
--   without narrowing what this function accepts.
fieldNamesOf :: forall a. GFieldNames (Rep a) => Proxy a -> [Text]
fieldNamesOf _ = gFieldNames (Proxy @(Rep a))

--------------------------------------------------------------------------------
-- Part 2: scan raw template text for top-level tag identifiers.

-- | Every top-level @{{ field }}@\/@{{{ field }}}@\/@{{# field }}@\/
--   @{{^ field }}@ reference in a template, paired with its 1-based
--   line number. Closing tags (@{{\/ field }}@), comments
--   (@{{! ... }}@), and partials (@{{> name }}@) are recognised and
--   skipped, not misparsed as field references. The implicit
--   self-reference @{{.}}@ and any dotted path (@{{ a.b }}@) are also
--   skipped — see this module's Haddock for why.
scanTemplateFields :: Text -> [(Text, Int)]
scanTemplateFields = go 1
  where
    go :: Int -> Text -> [(Text, Int)]
    go _ t | T.null t = []
    go line t =
      let (before, rest) = T.breakOn "{{" t
      in if T.null rest
         then []
         else
           let lineHere = line + T.count "\n" before
               afterOpen = T.drop 2 rest
           in case parseTag afterOpen of
                Nothing -> go lineHere (T.drop 2 rest)
                Just (mIdent, remaining) ->
                  let consumed = T.take (T.length afterOpen - T.length remaining) afterOpen
                      linesInTag = T.count "\n" consumed
                      here = [ (ident, lineHere) | Just ident <- [mIdent] ]
                  in here ++ go (lineHere + linesInTag) remaining

    -- Parse one tag's content, starting just after its opening "{{".
    -- Returns the identifier referenced (if this tag kind names one
    -- at all) and the text remaining just after this tag's closing
    -- delimiter.
    parseTag :: Text -> Maybe (Maybe Text, Text)
    parseTag t0 =
      let t1 = T.dropWhile (== ' ') t0
      in case T.uncons t1 of
        Just ('{', rest) -> let (inner, after) = T.breakOn "}}}" rest
                             in Just (identOf inner, T.drop 3 after)
        Just ('/', rest) -> closing rest
        Just ('!', rest) -> closing rest
        Just ('>', rest) -> closing rest
        Just ('#', rest) -> withIdent rest
        Just ('^', rest) -> withIdent rest
        Just ('&', rest) -> withIdent rest
        _                -> withIdent t1
      where
        closing rest = let (_, after) = T.breakOn "}}" rest in Just (Nothing, T.drop 2 after)
        withIdent rest = let (inner, after) = T.breakOn "}}" rest
                          in Just (identOf inner, T.drop 2 after)
        identOf inner =
          let name = T.strip inner
          in if T.null name || name == "." || T.any (== '.') name
             then Nothing
             else Just name

--------------------------------------------------------------------------------
-- Part 3: putting it together.

-- | Context keys 'SARA.Internal.Planner.genRender' always injects
--   into a template's render context, regardless of what fields the
--   page's own typed metadata has — see that module's context-building
--   code for the authoritative source this list mirrors.
alwaysAvailableContextKeys :: [Text]
alwaysAvailableContextKeys = ["itemBody", "siteTitle", "siteUrl", "siteAuthor"]

-- | Every field a template references that isn't a known field of
--   @meta@ and isn't one of 'alwaysAvailableContextKeys'. Each
--   distinct @(field, line)@ pair is reported once — a field
--   mistakenly referenced twice on the very same line collapses to a
--   single entry, but the same typo repeated across different lines
--   is reported once per line, deliberately: seeing every line a
--   typo needs fixing on is more useful to 'SARA.DSL.renderTyped's
--   caller than silently dropping all but the first.
unknownTemplateFields :: [Text] -> Text -> [(Text, Int)]
unknownTemplateFields knownFields templateText =
  nub [ (name, line) | (name, line) <- scanTemplateFields templateText, name `notElem` known ]
  where
    known = knownFields ++ alwaysAvailableContextKeys
