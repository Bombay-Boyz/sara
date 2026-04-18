{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE DataKinds #-}
module SARA.Migration.Hugo
  ( translateHugoShortcodes
  ) where

import SARA.Error (SaraError(..), SaraErrorKind(..))
import Data.Text (Text)
import qualified Data.Text as T

-- | Translates common Hugo shortcodes to Markdown/SARA equivalents.
translateHugoShortcodes
  :: FilePath
  -> Text
  -> Either (SaraError 'EKMigration) Text
translateHugoShortcodes _ content =
  Right $ translateHugoHighlight
        $ translateHugoRef content

-- {{< highlight go >}} -> ```go
-- {{< /highlight >}} -> ```
translateHugoHighlight :: Text -> Text
translateHugoHighlight t =
  T.replace "{{< /highlight >}}" "```" $
  T.replace "{{% /highlight %}}" "```" $
  let go text =
        let (before, match) = T.breakOn "{{< highlight " text
        in if T.null match
           then 
             let (before2, match2) = T.breakOn "{{% highlight " text
             in if T.null match2 then text else translateOne match2 before2
           else translateOne match before
      translateOne match before =
        let rest = T.drop (T.length "{{< highlight ") match
            (lang, after) = T.breakOn " >}}" rest
            -- handle % variant
            (lang2, after2) = if T.null after 
                              then T.breakOn " %}}" (T.drop (T.length "{{% highlight ") match)
                              else (lang, after)
            actualLang = if T.null after then lang2 else lang
            actualAfter = if T.null after then T.drop 3 after2 else T.drop 4 after
            fence = "```" <> T.strip actualLang
        in before <> fence <> go actualAfter
  in go t

-- {{< ref "path" >}} -> [ref](path)
translateHugoRef :: Text -> Text
translateHugoRef t =
  let go text =
        let (before, match) = T.breakOn "{{< ref " text
        in if T.null match
           then text
           else 
             let rest = T.drop (T.length "{{< ref \"") match
                 (path, after) = T.breakOn "\" >}}" rest
                 link = "[ref](" <> T.strip path <> ")"
             in before <> link <> go (T.drop 4 after)
  in go t
