# Building this repo

## If you have normal internet/Hackage access (most people)

```
cabal build
cabal test
```

should both just work, and both have been verified end-to-end in this
sandbox: the full library, executable, and 80-module test suite (156
tests) compile and pass — the only skip is one end-to-end test that
itself shells out to `cabal list-bin`, which needs real Hackage access
this sandbox doesn't have (see below).

`cabal.project` is a plain, unrestricted config — `packages: .` and
nothing else. `sara.cabal`'s dependency list uses only real Hackage
packages (see the substitutions note below for why a couple of names
differ from an earlier version of this codebase).

### Regex support needs no system library at all

`SARA.Security.RegexGuard`/`SARA.Routing.Engine` are built on
`regex-tdfa`, a pure-Haskell, automaton-based (tagged DFA) regex
engine. No C library of any kind — no system `libpcre`/`libpcre2`, no
`pkg-config` entry, no distro-specific package name to chase, ever.

This project went through three regex backends before landing here —
`regex-pcre-builtin` (Hackage-only, unreachable in some build
environments), `regex-pcre` (needs system PCRE1, which current Debian
no longer packages at all), and `pcre2` (bundles PCRE2's C sources, so
no *system* library, but still a backtracking engine, meaning a
pathological pattern can still exhibit exponential-time matching) —
each closing one problem while leaving a different one open. See
`BUILD_AND_FIXES_SUMMARY.md`'s "Regex library history" section for the
full account, including a real, meaningful trade-off worth knowing:
`regex-tdfa` implements POSIX Extended Regular Expressions, not PCRE,
so lookahead/lookbehind, in-pattern backreferences, and PCRE-style
named capture groups aren't available in `regexRoute` patterns.

A genuine bonus, not just a portability fix: automaton-based matching
runs in time linear in input length for *any* pattern, so ReDoS
(catastrophic backtracking) is structurally impossible with this
backend — not mitigated by heuristics, actually impossible. The
pattern-shape heuristics `SARA.Security.RegexGuard` used to carry
under the backtracking backends (nested-quantifier detection,
alternation-in-repetition detection) have been removed entirely, since
the vulnerability class they were guessing at doesn't exist here.

## If `cabal build` still fails for you

A handful of dependencies were substituted from the versions this codebase
may have used previously, because the environment this repo was fixed in
had no route to Hackage at all (sandboxed CI-style environment, apt-only).
Each substitute is a real, ordinary Hackage package, so this should not
affect you if you have normal internet access — but if you're also in a
restricted environment, see `BUILD_AND_FIXES_SUMMARY.md`'s dependency
substitution table for what changed and why:

- `blake3` → `cryptohash-sha256`
- `JuicyPixels-extra` → inlined (one function, reimplemented locally)
- `regex-pcre-builtin` → `regex-tdfa` (see history above)
- `stache` → `mustache`
- `toml-parser` → inlined (`SARA.Internal.Toml`, a small local parser)

If you specifically need the *original* dependency set (e.g. you're
maintaining a fork and want `toml-parser` back for its fuller TOML 1.0
compliance, or need real PCRE syntax for `regexRoute` patterns), those
are straightforward package-for-package swaps back in `sara.cabal` plus
reverting the small number of call sites listed in
`BUILD_AND_FIXES_SUMMARY.md`.

## Building without cabal at all (e.g. still no Hackage access)

`ghc --make` works directly against a GHC package database populated via
apt (`apt-get install libghc-*-dev ...` for each dependency in
`sara.cabal`'s `build-depends`), with no `cabal.project` involved. Every
dependency in the current `sara.cabal`, including `regex-tdfa`, is
apt-packaged (`libghc-regex-tdfa-dev`), so this path is complete —
unlike the brief period this codebase depended on `pcre2`, which is not.

```
ghc --make -isrc -iapp \
  -XGHC2021 -XDataKinds -XGADTs -XKindSignatures -XLambdaCase \
  -XOverloadedStrings -XScopedTypeVariables -XStrictData -XTupleSections -XTypeFamilies \
  $(for p in $(awk '/^  build-depends:/{f=1;next}/^[a-z]/{f=0}f' sara.cabal \
      | grep -oE '^\s*[A-Za-z][A-Za-z0-9_-]*' | sed -n '2,$p'); do printf -- "-package %s " "$p"; done) \
  -package temporary \
  -o sara app/Main.hs
```

This is exactly how the shipped `sara` binary in this deliverable was built.

### Building and running the test suite without cabal

The test suite's `test/Spec.hs` uses `hspec-discover` as a GHC
preprocessor (`-F -pgmF hspec-discover`) to auto-generate its own
`main`, scanning `test/` for `*Spec.hs` files at build time. `ghc
--make`'s `-F -pgmF` flag applies to every file in the build, not just
the one that needs it, so it can't be used as a drop-in replacement for
`cabal test` the way the executable build above can. To build the test
suite without cabal, generate the driver once with `hspec-discover`
directly, then compile that file normally (no `-F` needed):

```
hspec-discover test test test/GeneratedMain.hs --module-name=Main
ghc --make -isrc -itest [same flags/-package list as above, plus] \
  -package hspec -package QuickCheck -package hedgehog -package hspec-hedgehog \
  -o sara-test test/GeneratedMain.hs
```
