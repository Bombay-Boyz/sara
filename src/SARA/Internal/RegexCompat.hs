-- | A thin wrapper over @regex-tdfa@'s @Text@-native API.
--
--   __Why @regex-tdfa@__: this codebase went through three regex
--   backends before landing here — @regex-pcre-builtin@
--   (Hackage-only, unreachable in some build environments),
--   @regex-pcre@ (needs the system PCRE1 library, which current
--   Debian no longer packages at all), and @pcre2@ (bundles its own
--   PCRE2 C sources, so no system library — but still a backtracking
--   engine, meaning a pathological pattern can still exhibit
--   exponential-time matching, which is exactly the class of problem
--   'SARA.Security.RegexGuard' used to carry a whole module of
--   heuristics (@checkComplexity@, @hasNestedQuantifiers@, ...) to
--   *guess at detecting* — its own comments said outright that "a
--   full regex parser for safety is out of scope."
--
--   @regex-tdfa@ is a tagged-DFA (automaton-based) engine, pure
--   Haskell, no C dependency of any kind. Two consequences follow:
--
--   1. __No system library, ever, on any distro__ — this is the
--      "future-proof" property the last two backends were each
--      supposed to provide and didn't.
--   2. __ReDoS is not just mitigated but structurally impossible__:
--      automaton-based matching runs in time linear in input length
--      regardless of pattern shape. There is no pattern a
--      'SARA.Types.SafeRegex' can wrap that can make matching take
--      exponential time, which is why the heuristic detection module
--      this file's comment used to point at no longer exists — the
--      vulnerability class it was guessing at is gone, not better
--      guessed-at.
--
--   __Trade-off, stated plainly__: @regex-tdfa@ implements POSIX
--   Extended Regular Expressions, not PCRE. Patterns using
--   lookahead\/lookbehind, in-pattern backreferences, or named
--   capture groups in PCRE's @(?\<name\>...)@ syntax are not
--   supported and will fail to compile; POSIX's leftmost-longest
--   alternation semantics can also pick a different match than PCRE's
--   leftmost-first would for a genuinely ambiguous pattern. For
--   'SARA.Routing.Engine'\'s @regexRoute@ feature — matching a source
--   path to derive an output route — this is a real but narrow
--   surface: none of those PCRE-only features are needed to express
--   "match this path shape, capture these segments."
module SARA.Internal.RegexCompat
  ( Regex
  , compileRegexText
  , matchCaptures
  ) where

import Data.Array (elems)
import Data.Text (Text)
import Text.Regex.Base.RegexLike (makeRegexOptsM, matchOnce)
import Text.Regex.TDFA (defaultCompOpt, defaultExecOpt)
import Text.Regex.TDFA.Text (Regex)

-- | Compile a pattern; 'Nothing' iff the pattern is not valid POSIX
--   ERE. Pure — @regex-tdfa@ has no separate 'IO'-typed compile step
--   the way @pcre2@ did, so 'SARA.Security.RegexGuard.mkSafeRegex' can
--   go back to being a plain, total function.
compileRegexText :: Text -> Maybe Regex
compileRegexText = makeRegexOptsM defaultCompOpt defaultExecOpt

-- | Match a compiled pattern against a subject, returning every
--   capture's @(offset, length)@ in 'Text' index units — index 0 is
--   the whole match, followed by each parenthesised group — or
--   'Nothing' if the pattern doesn't match at all.
matchCaptures :: Regex -> Text -> Maybe [(Int, Int)]
matchCaptures re subject = elems <$> matchOnce re subject
