-- | The Porter stemming algorithm (Porter, 1980, "An algorithm for
--   suffix stripping", Program 14(3):130-137), implemented directly
--   against the author's own canonical reference — both the
--   human-readable specification
--   (https://tartarus.org/martin/PorterStemmer/def.txt) and his
--   reference C implementation
--   (https://tartarus.org/martin/PorterStemmer/c.txt), which the
--   author documents as differing from the published paper at two
--   specific points ("DEPARTURE"s — see 'step2' below) because they
--   are, in his words, "definitely improvements". This module follows
--   the C reference exactly, including those two departures, rather
--   than the strictly-1980-published version, since the C reference
--   is the version every modern stemmer implementation (NLTK, Snowball,
--   etc.) actually reproduces and tests against.
--
--   __Why this exists__: 'SARA.Search.Index' previously indexed raw,
--   unstemmed word forms — "connect", "connected", "connecting", and
--   "connection" were four unrelated index tokens instead of one.
--   Stemming conflates them, which is the entire point of running a
--   stemmer ahead of an inverted search index in the first place (see
--   the algorithm's own introduction, quoted in spirit above).
--
--   __Correctness__: every branch below was hand-verified against the
--   worked examples given directly in the specification itself
--   (@caresses -> caress@, @agreed -> agree@, @relational -> relate@,
--   @controll -> control@, and so on through all five steps), plus two
--   full multi-step traces the specification walks through explicitly
--   (@generalizations -> gener@, @oscillators -> oscil@). The test
--   suite (@SARA.Search.StemmerSpec@) encodes these same examples as
--   executable tests, rather than leaving this module's correctness
--   resting on a one-time hand-check.
--
--   One trap worth naming explicitly, since it cost real debugging
--   time here: the specification's per-step worked examples (e.g.
--   Step 2's @relational -> relate@) show only what /that step/ does
--   in isolation — later steps still run afterward on that output.
--   The actual, complete stem of \"relational\" is @relat@, not
--   @relate@ (step 5a removes the trailing E once the word reaches
--   it, since the stem \"relat\" has measure 2). This is independently
--   confirmed by NLTK's @PorterStemmer@, the most widely used
--   reference implementation, which produces the same @relat@,
--   @troubl@, and @hopefulness -> hope@ results. Treat every one of
--   this specification's inline examples as "what step N alone does",
--   never as "what the whole algorithm produces" unless it is
--   explicitly one of the two full worked traces.
module SARA.Search.Stemmer
  ( stem
  ) where

import qualified Data.List.NonEmpty as NE
import Data.Text (Text)
import qualified Data.Text as T

-- | Stem a single word. Case-insensitive (lower-cases internally, matching
--   the reference implementation's documented precondition that only
--   lower-case input is meaningful to stem). Total: never throws, and
--   words of length 1 or 2 are returned unchanged, exactly as the
--   reference implementation's own initial guard specifies — this
--   isn't mentioned in the 1980 paper's prose, but is explicit in the
--   author's C reference and its accompanying comment.
stem :: Text -> Text
stem w0
  | T.length w <= 2 = w
  | otherwise =
      let afterStep1 = step1c (step1ab w)
      in if T.length afterStep1 >= 2
         then step5b (step5a (step4 (step3 (step2 afterStep1))))
         else afterStep1
  where
    w = T.toLower w0

--------------------------------------------------------------------------------
-- Core primitives: consonant/vowel classification and "measure"

-- | Per-position consonant/vowel classification, following the
--   algorithm's definition exactly: a letter is a consonant unless
--   it's A/E/I/O/U, or unless it's Y preceded by a consonant — so Y
--   at the very start of a word, or Y preceded by a vowel, counts as
--   a consonant; Y preceded by a consonant counts as a vowel. (Check
--   against the specification's own example: in SYZYGY, the
--   consonants are S, Z, and G — matched exactly by this definition.)
--   True = consonant, False = vowel; one entry per character, in order.
consonantFlags :: Text -> [Bool]
consonantFlags = go Nothing . T.unpack
  where
    go :: Maybe Bool -> String -> [Bool]
    go _ [] = []
    go prev (c : cs)
      | c `elem` ("aeiou" :: String) = False : go (Just False) cs
      | c == 'y' =
          let isCons = maybe True not prev
          in isCons : go (Just isCons) cs
      | otherwise = True : go (Just True) cs

-- | The algorithm's "measure" (m): the number of vowel-run-to-
--   consonant-run transitions in @[C](VC)^m[V]@. Any single trailing
--   vowel (as when this is called on a word still carrying a suffix's
--   final vowel) never changes this count, since a trailing vowel can
--   only extend or start a final run with nothing after it to
--   transition to — so it is safe and equivalent to call this on a
--   stem with or without a subsequently-removed trailing vowel.
measure :: Text -> Int
measure = countTransitions . collapseRuns . consonantFlags
  where
    -- 'Data.List.group' never produces an empty sublist (each group
    -- is, by construction, at least one element), so 'NE.head' here
    -- is total in practice — used instead of the partial
    -- 'Prelude.head' so that's true by the types, not just by
    -- argument.
    collapseRuns = map NE.head . NE.group
    countTransitions rt = length [ () | (isV, isC) <- zip rt (drop 1 rt), not isV, isC ]

-- | Does the given text contain at least one vowel, by this
--   algorithm's positional (Y-aware) definition?
containsVowel :: Text -> Bool
containsVowel = any not . consonantFlags

-- | Does the text end in a double consonant (e.g. @-TT@, @-SS@)?
endsDoubleConsonant :: Text -> Bool
endsDoubleConsonant t =
  T.length t >= 2
    && T.last t == T.last (T.dropEnd 1 t)
    && lastIsConsonant
  where
    lastIsConsonant = case reverse (consonantFlags t) of
      (f : _) -> f
      []      -> False

-- | Does the text end \"cvc\" — consonant, vowel, consonant — where
--   the final consonant is not W, X, or Y? (Used to decide whether a
--   short word like @fil@ should regain a trailing E to become
--   @file@, while @fail@ should not.)
endsCVC :: Text -> Bool
endsCVC t = case reverse (consonantFlags t) of
  (isLastCons : isPenultCons : isAntepenultCons : _) ->
    isLastCons && not isPenultCons && isAntepenultCons
      && T.last t `notElem` ("wxy" :: String)
  _ -> False

--------------------------------------------------------------------------------
-- Step 1a/1b: plurals and past participles/present participles

step1ab :: Text -> Text
step1ab = step1ab_2 . step1ab_1

-- | Step 1a: SSES -> SS, IES -> I, SS -> SS (no change), S -> (removed).
step1ab_1 :: Text -> Text
step1ab_1 w
  | T.isSuffixOf "s" w =
      if T.isSuffixOf "sses" w
        then T.dropEnd 2 w
        else case T.stripSuffix "ies" w of
          Just s -> s <> "i"
          Nothing ->
            if T.length w >= 2 && T.index w (T.length w - 2) /= 's'
              then T.dropEnd 1 w
              else w
  | otherwise = w

-- | Step 1b: (m>0) EED -> EE; (*v*) ED -> ; (*v*) ING -> ; with the
--   AT/BL/IZ/double-consonant/cvc tidy-up that follows a successful
--   ED or ING removal.
step1ab_2 :: Text -> Text
step1ab_2 w =
  case T.stripSuffix "eed" w of
    Just s | measure s > 0 -> s <> "ee"
    Just _ -> w
    Nothing ->
      case edOrIngStem w of
        Nothing -> w
        Just afterSuffix
          | T.isSuffixOf "at" afterSuffix -> afterSuffix <> "e"
          | T.isSuffixOf "bl" afterSuffix -> afterSuffix <> "e"
          | T.isSuffixOf "iz" afterSuffix -> afterSuffix <> "e"
          | endsDoubleConsonant afterSuffix ->
              let dropped = T.dropEnd 1 afterSuffix
              in if not (T.null dropped) && T.last dropped `elem` ("lsz" :: String)
                 then afterSuffix
                 else dropped
          | measure afterSuffix == 1 && endsCVC afterSuffix -> afterSuffix <> "e"
          | otherwise -> afterSuffix
  where
    edOrIngStem t =
      case T.stripSuffix "ed" t of
        Just s | containsVowel s -> Just s
        _ -> case T.stripSuffix "ing" t of
          Just s | containsVowel s -> Just s
          _ -> Nothing

-- | Step 1c: (*v*) Y -> I.
step1c :: Text -> Text
step1c w = case T.stripSuffix "y" w of
  Just s | containsVowel s -> s <> "i"
  _ -> w

--------------------------------------------------------------------------------
-- Steps 2-4: generic "longest matching suffix, replace if measure
-- threshold met, otherwise leave completely unchanged" rule application.
-- Matching a suffix commits to that rule even if its measure check then
-- fails — matching the reference's switch/break structure, where a
-- case is chosen the moment its ends() check succeeds, whether or not
-- the m()-gated replacement inside it actually fires.

applyRules :: [(Text, Text)] -> Int -> Text -> Text
applyRules rules minMeasure w = go rules
  where
    go [] = w
    go ((suf, repl) : rest) =
      case T.stripSuffix suf w of
        Just s | measure s > minMeasure -> s <> repl
               | otherwise -> w
        Nothing -> go rest

-- | Step 2: double suffixes collapse to single ones (@-IZATION@ is
--   @-IZE@ plus @-ATION@, etc). Includes both of the reference
--   implementation's documented departures from the 1980 paper:
--   @-BLI -> -BLE@ (rather than requiring the narrower @-ABLI ->
--   -ABLE@), and an added @-LOGI -> -LOG@ case absent from the
--   original paper entirely. Order matters here — e.g. @-ATIONAL@ is
--   checked before the @-TIONAL@ it would otherwise also match as a
--   substring — and is preserved exactly as the reference orders them.
step2 :: Text -> Text
step2 = applyRules
  [ ("ational", "ate")
  , ("tional", "tion")
  , ("enci", "ence")
  , ("anci", "ance")
  , ("izer", "ize")
  , ("bli", "ble")     -- DEPARTURE (published: "abli" -> "able")
  , ("alli", "al")
  , ("entli", "ent")
  , ("eli", "e")
  , ("ousli", "ous")
  , ("ization", "ize")
  , ("ation", "ate")
  , ("ator", "ate")
  , ("alism", "al")
  , ("iveness", "ive")
  , ("fulness", "ful")
  , ("ousness", "ous")
  , ("aliti", "al")
  , ("iviti", "ive")
  , ("biliti", "ble")
  , ("logi", "log")    -- DEPARTURE (absent from the published paper)
  ]
  0

-- | Step 3: further suffix simplification (@-ICATE@, @-FUL@, @-NESS@, ...).
step3 :: Text -> Text
step3 = applyRules
  [ ("icate", "ic")
  , ("ative", "")
  , ("alize", "al")
  , ("iciti", "ic")
  , ("ical", "ic")
  , ("ful", "")
  , ("ness", "")
  ]
  0

-- | Step 4: strip suffixes needing measure > 1 (@-ANT@, @-ENCE@,
--   @-ABLE@, ...). @-ION@ carries an extra condition absent from every
--   other rule here — the stem must itself end in S or T — so it's
--   handled with its own predicate rather than forced through
--   'applyRules'.
step4 :: Text -> Text
step4 w = go rules
  where
    go [] = w
    go ((suf, extraOk) : rest) =
      case T.stripSuffix suf w of
        Nothing -> go rest
        Just s
          | measure s > 1 && extraOk s -> s
          | otherwise -> w
    endsInST s = T.isSuffixOf "s" s || T.isSuffixOf "t" s
    rules =
      [ ("al", const True)
      , ("ance", const True)
      , ("ence", const True)
      , ("er", const True)
      , ("ic", const True)
      , ("able", const True)
      , ("ible", const True)
      , ("ant", const True)
      , ("ement", const True)
      , ("ment", const True)
      , ("ent", const True)
      , ("ion", endsInST)
      , ("ou", const True)
      , ("ism", const True)
      , ("ate", const True)
      , ("iti", const True)
      , ("ous", const True)
      , ("ive", const True)
      , ("ize", const True)
      ]

--------------------------------------------------------------------------------
-- Step 5: final tidy-up

-- | Step 5a: remove a final E if measure > 1, or if measure == 1 and
--   the stem (without the E) doesn't end \"cvc\".
step5a :: Text -> Text
step5a w = case T.stripSuffix "e" w of
  Nothing -> w
  Just s ->
    let a = measure s
    in if a > 1 || (a == 1 && not (endsCVC s))
       then s
       else w

-- | Step 5b: @-LL -> -L@ if measure > 1.
step5b :: Text -> Text
step5b w
  | T.isSuffixOf "l" w && endsDoubleConsonant w && measure w > 1 = T.dropEnd 1 w
  | otherwise = w
