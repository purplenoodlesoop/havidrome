-- | What an item reads as on its line: the text an artist, an album or a song
-- gives its column, and the mark the song playback is on carries.
module Havidrome.Browse.RowSpec (spec) where

import Data.Text qualified as T
import Havidrome.Browse.Fixtures (album, artist, song)
import Havidrome.Browse.Row (Row (row), mark, marking)
import Havidrome.Subsonic.Types (SongId (SongId))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "row" $ do
    it "shows an artist by name" $
      row (artist "a" "Aphex Twin") `shouldBe` "Aphex Twin"

    it "shows an album's year before its name" $
      row (album "b" "Drukqs" (Just 2001)) `shouldBe` "2001  Drukqs"

    it "leaves the column blank for an album the server gave no year" $
      row (album "b" "Sketches" Nothing) `shouldBe` "      Sketches"

    it "shows a song's track number before its title, the mark's column blank between them" $
      row (song "s" "Vordhosbn" 293 (Just 2)) `shouldBe` "  2   Vordhosbn"

    it "leaves the column blank for a song the server gave no track number" $
      row (song "s" "Btoum Roumada" 96 Nothing) `shouldBe` "      Btoum Roumada"

  describe "marking" $ do
    it "puts the mark and one space before the name of the song playback is on" $
      marking (Just (SongId "s")) (song "s" "Vordhosbn" 293 (Just 2))
        `shouldBe` "  2 " <> mark <> " Vordhosbn"

    it "keeps the name in line with the unmarked rows around it" $
      T.length (marking (Just (SongId "s")) (song "s" "Vordhosbn" 293 (Just 2)))
        `shouldBe` T.length (row (song "s" "Vordhosbn" 293 (Just 2)))

    it "leaves every other song as its row" $ do
      marking (Just (SongId "t")) (song "s" "Vordhosbn" 293 (Just 2)) `shouldBe` "  2   Vordhosbn"
      marking Nothing (song "s" "Vordhosbn" 293 (Just 2)) `shouldBe` "  2   Vordhosbn"
