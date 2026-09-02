{-# LANGUAGE OverloadedStrings #-}

-- | Every case here is transcribed directly from the worked examples
--   in Porter's own specification
--   (https://tartarus.org/martin/PorterStemmer/def.txt) and reference
--   C implementation (https://tartarus.org/martin/PorterStemmer/c.txt)
--   — not invented — so a passing suite here is a direct check against
--   the algorithm's own author, not just against 'SARA.Search.Stemmer'
--   agreeing with itself.
module SARA.StemmerSpec (spec) where

import Test.Hspec
import SARA.Search.Stemmer (stem)

spec :: Spec
spec = describe "SARA.Search.Stemmer" $ do

  describe "Step 1a: plurals" $ do
    it "caresses -> caress" $ stem "caresses" `shouldBe` "caress"
    it "ponies -> poni"     $ stem "ponies"   `shouldBe` "poni"
    it "ties -> ti"         $ stem "ties"     `shouldBe` "ti"
    it "caress -> caress (unchanged)" $ stem "caress" `shouldBe` "caress"
    it "cats -> cat"        $ stem "cats"     `shouldBe` "cat"

  describe "Step 1b: -eed/-ed/-ing and the tidy-up that follows" $ do
    it "feed -> feed (unchanged, m=0)" $ stem "feed"      `shouldBe` "feed"
    it "agreed -> agre (step 1 gives \"agree\"; step 5a then removes the trailing E, m=1 and no cvc)" $
      stem "agreed" `shouldBe` "agre"
    it "plastered -> plaster"          $ stem "plastered" `shouldBe` "plaster"
    it "bled -> bled (unchanged, no vowel in stem)" $ stem "bled" `shouldBe` "bled"
    it "motoring -> motor"             $ stem "motoring"  `shouldBe` "motor"
    it "sing -> sing (unchanged, no vowel in stem)" $ stem "sing" `shouldBe` "sing"
    it "conflated -> conflat (step 1 gives \"conflate\"; step 5a then removes the trailing E)" $
      stem "conflated" `shouldBe` "conflat"
    it "troubled -> troubl (step 1 gives \"trouble\"; step 5a then removes the trailing E — matches NLTK's PorterStemmer().stem(\"trouble\") == \"troubl\" exactly)" $
      stem "troubled" `shouldBe` "troubl"
    it "sized -> size (IZ tidy-up; m=2 after removing E in step 5a would need m>1 or cvc — here IZE's stem \"siz\" has m=1 and cvc holds, so E survives)" $
      stem "sized" `shouldBe` "size"
    it "hopping -> hop (double consonant collapses)" $ stem "hopping" `shouldBe` "hop"
    it "tanned -> tan"                               $ stem "tanned"  `shouldBe` "tan"
    it "falling -> fall (double L is preserved)"     $ stem "falling" `shouldBe` "fall"
    it "hissing -> hiss (double S is preserved)"     $ stem "hissing" `shouldBe` "hiss"
    it "fizzed -> fizz (double Z is preserved)"      $ stem "fizzed"  `shouldBe` "fizz"
    it "failing -> fail (no cvc, no augmentation)"   $ stem "failing" `shouldBe` "fail"
    it "filing -> file (cvc restores trailing E)"    $ stem "filing"  `shouldBe` "file"

  describe "Step 1c: terminal Y" $ do
    it "happy -> happi" $ stem "happy" `shouldBe` "happi"
    it "sky -> sky (unchanged, no vowel before the y)" $ stem "sky" `shouldBe` "sky"

  describe "Step 2: double suffixes (further reduced by steps 3-5, same as the spec's own step-3/4 walkthrough for GENERALIZATIONS)" $ do
    it "relational -> relat (matches the commonly-cited example of Porter's stemmer producing a non-word: 'relational'->'relat')" $
      stem "relational" `shouldBe` "relat"
    it "conditional -> condit" $ stem "conditional" `shouldBe` "condit"
    it "rational -> ration (ATIONAL doesn't fire, m=0 on stem \"r\"; step 4's plain -AL rule then fires on \"rational\" itself, m=2)" $
      stem "rational" `shouldBe` "ration"
    it "valenci -> valenc"     $ stem "valenci"     `shouldBe` "valenc"
    it "hesitanci -> hesit"    $ stem "hesitanci"   `shouldBe` "hesit"
    it "digitizer -> digit"    $ stem "digitizer"   `shouldBe` "digit"
    it "conformabli -> conform (BLI departure)" $ stem "conformabli" `shouldBe` "conform"
    it "radicalli -> radic"     $ stem "radicalli"     `shouldBe` "radic"
    it "differentli -> differ"  $ stem "differentli"   `shouldBe` "differ"
    it "vileli -> vile"         $ stem "vileli"        `shouldBe` "vile"
    it "analogousli -> analog"  $ stem "analogousli"   `shouldBe` "analog"
    it "vietnamization -> vietnam" $ stem "vietnamization" `shouldBe` "vietnam"
    it "predication -> predic"  $ stem "predication"   `shouldBe` "predic"
    it "operator -> oper"       $ stem "operator"      `shouldBe` "oper"
    it "feudalism -> feudal"    $ stem "feudalism"     `shouldBe` "feudal"
    it "decisiveness -> decis"  $ stem "decisiveness"  `shouldBe` "decis"
    it "hopefulness -> hope (matches the documented two-phase example: hopefulness -> hopeful (step 2) -> hope (step 3))" $
      stem "hopefulness" `shouldBe` "hope"
    it "callousness -> callous" $ stem "callousness"   `shouldBe` "callous"
    it "formaliti -> formal"    $ stem "formaliti"     `shouldBe` "formal"
    it "sensitiviti -> sensit"  $ stem "sensitiviti"   `shouldBe` "sensit"
    it "sensibiliti -> sensibl" $ stem "sensibiliti"   `shouldBe` "sensibl"

  describe "Step 3" $ do
    it "triplicate -> triplic" $ stem "triplicate" `shouldBe` "triplic"
    it "formative -> form"     $ stem "formative"  `shouldBe` "form"
    it "formalize -> formal"   $ stem "formalize"  `shouldBe` "formal"
    it "electriciti -> electr (further reduced past step 3's own \"electric\" by step 4's -IC rule, m>1)" $
      stem "electriciti" `shouldBe` "electr"
    it "electrical -> electr" $ stem "electrical" `shouldBe` "electr"
    it "hopeful -> hope"       $ stem "hopeful"    `shouldBe` "hope"
    it "goodness -> good"      $ stem "goodness"   `shouldBe` "good"

  describe "Step 4" $ do
    it "revival -> reviv"       $ stem "revival"     `shouldBe` "reviv"
    it "allowance -> allow"     $ stem "allowance"   `shouldBe` "allow"
    it "inference -> infer"     $ stem "inference"   `shouldBe` "infer"
    it "airliner -> airlin"     $ stem "airliner"    `shouldBe` "airlin"
    it "gyroscopic -> gyroscop" $ stem "gyroscopic"  `shouldBe` "gyroscop"
    it "adjustable -> adjust"   $ stem "adjustable"  `shouldBe` "adjust"
    it "defensible -> defens"   $ stem "defensible"  `shouldBe` "defens"
    it "irritant -> irrit"      $ stem "irritant"    `shouldBe` "irrit"
    it "replacement -> replac"  $ stem "replacement" `shouldBe` "replac"
    it "adjustment -> adjust"   $ stem "adjustment"  `shouldBe` "adjust"
    it "dependent -> depend"    $ stem "dependent"   `shouldBe` "depend"
    it "adoption -> adopt (ION requires stem ending in S/T)" $
      stem "adoption" `shouldBe` "adopt"
    it "homologou -> homolog"   $ stem "homologou"   `shouldBe` "homolog"
    it "communism -> commun"    $ stem "communism"   `shouldBe` "commun"
    it "activate -> activ"      $ stem "activate"    `shouldBe` "activ"
    it "angulariti -> angular"  $ stem "angulariti"  `shouldBe` "angular"
    it "homologous -> homolog"  $ stem "homologous"  `shouldBe` "homolog"
    it "effective -> effect"    $ stem "effective"   `shouldBe` "effect"
    it "bowdlerize -> bowdler"  $ stem "bowdlerize"  `shouldBe` "bowdler"

  describe "Step 5a: final E" $ do
    it "probate -> probat (m>1)" $ stem "probate" `shouldBe` "probat"
    it "rate -> rate (unchanged: m=1 and cvc holds)" $ stem "rate" `shouldBe` "rate"
    it "cease -> ceas (m=1, cvc doesn't hold)" $ stem "cease" `shouldBe` "ceas"

  describe "Step 5b: final double L" $ do
    it "controll -> control" $ stem "controll" `shouldBe` "control"
    it "roll -> roll (unchanged, m=1)" $ stem "roll" `shouldBe` "roll"

  describe "Multi-step compound examples (traced through all 5 steps in the spec itself)" $ do
    it "generalizations -> gener" $ stem "generalizations" `shouldBe` "gener"
    it "oscillators -> oscil"     $ stem "oscillators"     `shouldBe` "oscil"

  describe "Edge cases" $ do
    it "leaves 1-letter words unchanged" $ stem "a" `shouldBe` "a"
    it "leaves 2-letter words unchanged" $ stem "by" `shouldBe` "by"
    it "leaves the empty string unchanged" $ stem "" `shouldBe` ""
    it "is idempotent on an already-stemmed word" $ stem (stem "connections") `shouldBe` stem "connections"
    it "lower-cases its input" $ stem "CONNECTIONS" `shouldBe` stem "connections"

  describe "The motivating example from this module's own Haddock" $
    it "conflates the CONNECT family to a single stem" $ do
      let stems = map stem ["connect", "connected", "connecting", "connection", "connections"]
      case stems of
         []      -> expectationFailure "stems list was empty"
         (s:_)   -> stems `shouldSatisfy` all (== s)
