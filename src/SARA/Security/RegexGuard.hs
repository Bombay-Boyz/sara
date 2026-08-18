module SARA.Security.RegexGuard
  ( SafeRegex
  , unSafeRegex
  , mkSafeRegex
  ) where

import Data.Text (Text)
import SARA.Error (SaraError(..), SaraErrorKind(..))
import SARA.Internal.RegexCompat (compileRegexText)

-- | Opaque newtype for a regex pattern that has passed 'mkSafeRegex's
--   compile-validity check. The constructor is deliberately *not*
--   exported (see this type's export list, and issue #2 of the
--   security audit this module's containment guarantee is
--   load-bearing for) — 'mkSafeRegex' is the only way to produce a
--   value of this type anywhere outside this module, so a
--   type-checked 'SafeRegex' really is proof the check ran, not just
--   a value that happens to look validated.
newtype SafeRegex = SafeRegex { unSafeRegex :: Text }
  deriving (Eq, Show)

-- | Smart constructor for SafeRegex. Total and pure: unlike this
--   module's prior two backends, neither of which could offer that —
--   @regex-pcre@ required an unchecked recompile-and-hope at every
--   match site, and @pcre2@'s compile-validity check could only be
--   performed in 'IO' — @regex-tdfa@ exposes pattern compilation as a
--   plain, pure @Maybe@ (see 'SARA.Internal.RegexCompat.compileRegexText'),
--   so this can go back to being what it always should have been.
--
--   This function no longer runs any ReDoS-specific heuristics. The
--   version of this module written against a backtracking PCRE engine
--   (@regex-pcre@\/@pcre2@) carried a whole set of pattern-shape
--   heuristics here — nested-quantifier detection, alternation-in-
--   repetition detection, nesting-depth limits — each an attempt to
--   *guess* whether a pattern could exhibit catastrophic backtracking,
--   with the guesswork's own comment admitting "a full regex parser
--   for safety is out of scope." @regex-tdfa@ is an automaton-based
--   (tagged DFA) engine: matching runs in time linear in input length
--   for *any* pattern, so there is no pattern shape here to guess
--   about — the vulnerability class those heuristics were chasing
--   does not exist for this backend. The only way a pattern can now
--   be rejected is by failing to compile as valid POSIX ERE at all,
--   which 'compileRegexText' already reports precisely.
mkSafeRegex :: Text -> Either (SaraError 'EKSecurity) SafeRegex
mkSafeRegex t = case compileRegexText t of
  Nothing -> Left $ SecurityRegexInvalid t "Pattern is not a valid regular expression"
  Just _  -> Right (SafeRegex t)
