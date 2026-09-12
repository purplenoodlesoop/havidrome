-- | The measure every line is laid out against: what a character takes on a
-- terminal, what a line of them takes, and what is left of a line cut to fit.
module Havidrome.WidthSpec (spec) where

import Data.Char (isControl)
import Data.Text qualified as T
import Havidrome.Width (char, shorten, text)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (NonNegative))

spec :: Spec
spec = do
  describe "char" $ do
    it "gives a printable ASCII character one column" $
      fmap char "aZ0 ~!" `shouldBe` [1, 1, 1, 1, 1, 1]

    it "gives a control character none" $
      fmap char "\0\t\n\r\ESC\DEL" `shouldBe` [0, 0, 0, 0, 0, 0]

    it "gives a combining mark none, so it measures with what it hangs off" $ do
      fmap char "\x0301\x20DD\xFE0F" `shouldBe` [0, 0, 0]
      text "e\x0301" `shouldBe` 1

    it "gives an East Asian wide or full-width character two" $
      fmap char "音\x1100\xAC00\xFF21\x20000" `shouldBe` [2, 2, 2, 2, 2]

    it "gives the player's own glyphs one each" $
      fmap char "⏵⏸▶█░…⠋" `shouldBe` [1, 1, 1, 1, 1, 1, 1]

    it "gives a Latin letter outside ASCII one" $
      fmap char "äöüé" `shouldBe` [1, 1, 1, 1]

  describe "text" $ do
    it "is what its characters take between them" $
      text "音 a" `shouldBe` 4

    it "is nothing at all for nothing at all" $
      text "" `shouldBe` 0

  describe "shorten" $ do
    it "leaves text that fits as it is" $
      shorten 10 "Aphex Twin" `shouldBe` "Aphex Twin"

    it "cuts text that does not fit, and ends it in an ellipsis" $
      shorten 8 "Aphex Twin" `shouldBe` "Aphex T…"

    it "measures by the terminal's columns, splitting no wide character" $
      shorten 4 "日本語" `shouldBe` "日…"

    it "cuts by columns where the wide characters fill the room exactly" $
      shorten 5 "音音音音" `shouldBe` "音音…"

    it "leaves nothing where there is no room at all" $
      shorten 0 "Aphex Twin" `shouldBe` ""

    it "puts a space for whatever would move the terminal elsewhere" $
      shorten 20 "one\ntwo\tthree" `shouldBe` "one two three"

    prop "takes no more columns than it is given" $ \(NonNegative room) said ->
      text (shorten room (T.pack said)) <= room

    prop "leaves nothing that would move the terminal" $ \room said ->
      not (T.any isControl (shorten room (T.pack said)))
