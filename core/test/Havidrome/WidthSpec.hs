-- | The measure every line is laid out against: what a character takes on a
-- terminal, what a line of them takes, and what is left of a line cut to fit.
module Havidrome.WidthSpec (spec) where

import Data.Text qualified as Text
import Havidrome.Width (char, shorten, text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (Positive), (===))

spec :: Spec
spec = do
  describe "char" $ do
    it "gives a printable ASCII character one column" $
      map char "aZ0 ~!" `shouldBe` [1, 1, 1, 1, 1, 1]

    it "gives a control character none" $
      map char "\0\t\n\r\ESC\DEL" `shouldBe` [0, 0, 0, 0, 0, 0]

    it "gives a combining mark none, so it measures with what it hangs off" $ do
      map char "\x0301\x20DD\xFE0F" `shouldBe` [0, 0, 0]
      text "e\x0301" `shouldBe` 1

    it "gives an East Asian wide or full-width character two" $
      map char "音\x1100\xAC00\xFF21\x20000" `shouldBe` [2, 2, 2, 2, 2]

    it "gives the player's own glyphs one each" $
      map char "⏵⏸▶█░…⠋" `shouldBe` [1, 1, 1, 1, 1, 1, 1]

    it "gives a Latin letter outside ASCII one" $
      map char "äöüé" `shouldBe` [1, 1, 1, 1]

  describe "text" $ do
    it "is what its characters take between them" $
      text "音 a" `shouldBe` 4

    it "is nothing at all for nothing at all" $
      text "" `shouldBe` 0

  describe "shorten" $ do
    it "leaves a line that fits exactly as it is" $
      shorten 10 "abc" `shouldBe` "abc"

    it "cuts a line that does not, and says so with an ellipsis" $
      shorten 5 "abcdefgh" `shouldBe` "abcd…"

    it "cuts by columns, not by characters" $
      shorten 5 "音音音音" `shouldBe` "音音…"

    it "gives nothing at all for a line with no room even for the ellipsis" $
      shorten 0 "abc" `shouldBe` ""

    it "puts a space where a character would move the terminal elsewhere" $
      shorten 10 "a\nb\tc" `shouldBe` "a b c"

    prop "never takes more columns than it is given" $
      \(Positive room) said ->
        (text (shorten room (Text.pack said)) <= room) === True
