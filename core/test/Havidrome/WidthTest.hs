-- | The measure every line is laid out against: what a character takes on a
-- terminal, what a line of them takes, and what is left of a line cut to fit.
module Havidrome.WidthTest (tests) where

import Data.Char (isControl)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
import Havidrome.Width (char, shorten, text)
import Hedgehog (Gen, Group (Group), assert, diff, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests = Group "Havidrome.Width" (characters <> texts <> shortening)

-- | What one character takes on a terminal.
characters :: Checks
characters =
  [
    ( "char gives a printable ASCII character one column"
    , example (fmap char "aZ0 ~!" === [1, 1, 1, 1, 1, 1])
    )
  ,
    ( "char gives a control character none"
    , example (fmap char "\0\t\n\r\ESC\DEL" === [0, 0, 0, 0, 0, 0])
    )
  ,
    ( "char gives a combining mark none, so it measures with what it hangs off"
    , example do
        fmap char "\x0301\x20DD\xFE0F" === [0, 0, 0]
        text "e\x0301" === 1
    )
  ,
    ( "char gives an East Asian wide or full-width character two"
    , example (fmap char "音\x1100\xAC00\xFF21\x20000" === [2, 2, 2, 2, 2])
    )
  ,
    ( "char gives the player's own glyphs one each"
    , example (fmap char "⏵⏸▶█░…⠋" === [1, 1, 1, 1, 1, 1, 1])
    )
  ,
    ( "char gives a Latin letter outside ASCII one"
    , example (fmap char "äöüé" === [1, 1, 1, 1])
    )
  ]

-- | What a line of them takes together.
texts :: Checks
texts =
  [
    ( "text is what its characters take between them"
    , example (text "音 a" === 4)
    )
  ,
    ( "text is nothing at all for nothing at all"
    , example (text "" === 0)
    )
  ]

-- | What is left of a line cut to fit the room there is.
shortening :: Checks
shortening =
  [
    ( "shorten leaves text that fits as it is"
    , example (shorten 10 "Aphex Twin" === "Aphex Twin")
    )
  ,
    ( "shorten cuts text that does not fit, and ends it in an ellipsis"
    , example (shorten 8 "Aphex Twin" === "Aphex T…")
    )
  ,
    ( "shorten measures by the terminal's columns, splitting no wide character"
    , example (shorten 4 "日本語" === "日…")
    )
  ,
    ( "shorten cuts by columns where the wide characters fill the room exactly"
    , example (shorten 5 "音音音音" === "音音…")
    )
  ,
    ( "shorten leaves nothing where there is no room at all"
    , example (shorten 0 "Aphex Twin" === "")
    )
  ,
    ( "shorten puts a space for whatever would move the terminal elsewhere"
    , example (shorten 20 "one\ntwo\tthree" === "one two three")
    )
  ,
    ( "shorten takes no more columns than it is given"
    , property do
        room <- forAll (Gen.int (Range.linear 0 40))
        said <- forAll line
        diff (text (shorten room said)) (<=) room
    )
  ,
    ( "shorten leaves nothing that would move the terminal"
    , property do
        room <- forAll (Gen.int (Range.linearFrom 0 (-20) 40))
        said <- forAll line
        assert (not (T.any isControl (shorten room said)))
    )
  ]

-- | Any line whatsoever: the control characters a terminal would obey among
-- them, and the wide characters that take two columns rather than one.
line :: Gen Text
line = Gen.text (Range.linear 0 30) Gen.unicode
