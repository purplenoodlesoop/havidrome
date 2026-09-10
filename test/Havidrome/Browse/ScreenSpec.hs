{-# LANGUAGE OverloadedStrings #-}

-- | The browsing screen: what the keys do, and what the terminal shows.
module Havidrome.Browse.ScreenSpec (spec) where

import Data.Functor.Identity (runIdentity)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Graphics.Vty qualified as Vty
import Havidrome.Browse.Fixtures
  ( Answer
  , album
  , artist
  , artists
  , failing
  , library
  , song
  )
import Havidrome.Browse.Screen
  ( Command (Ascend, Descend, MoveDown, MoveUp, Quit)
  , Screen (browse, trouble)
  , command
  , draw
  , opening
  , row
  , step
  , theme
  , title
  )
import Havidrome.Library (Library)
import Havidrome.Subsonic (SubsonicError (NetworkFailure))
import Terminal (highlighted, screenshot)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "command" $ do
    it "moves the selection with the arrows" $ do
      command Vty.KUp [] `shouldBe` Just MoveUp
      command Vty.KDown [] `shouldBe` Just MoveDown

    it "moves it with j and k just the same" $ do
      command (Vty.KChar 'k') [] `shouldBe` Just MoveUp
      command (Vty.KChar 'j') [] `shouldBe` Just MoveDown

    it "descends on Enter and returns on Esc" $ do
      command Vty.KEnter [] `shouldBe` Just Descend
      command Vty.KEsc [] `shouldBe` Just Ascend

    it "quits on q" $
      command (Vty.KChar 'q') [] `shouldBe` Just Quit

    it "ignores every other key" $ do
      command (Vty.KChar 'x') [] `shouldBe` Nothing
      command Vty.KLeft [] `shouldBe` Nothing
      command (Vty.KChar 'q') [Vty.MCtrl] `shouldBe` Nothing

  describe "step" $ do
    it "leaves the player on q, at any level" $ do
      left Quit start `shouldSatisfy` isNothing
      left Quit (after Descend start) `shouldSatisfy` isNothing
      left Quit (after Descend (after Descend start)) `shouldSatisfy` isNothing

    it "descends into the level the library holds" $
      title (browse (after Descend start)) `shouldBe` "anohni"

    it "returns to the level above, still on what was descended into" $
      title (browse (after Ascend (after Descend (after MoveDown start))))
        `shouldBe` "Artists"

    it "keeps the level it is on when the library will not answer" $
      title (browse (stumbling Descend start)) `shouldBe` "Artists"

    it "says in the strip why the library did not answer" $
      trouble (stumbling Descend start)
        `shouldBe` Just "The server could not be reached: down"

    it "clears the strip on the next key press" $
      trouble (after MoveDown (stumbling Descend start)) `shouldBe` Nothing

  describe "row" $ do
    it "shows an artist by name" $
      row (artist "a" "Aphex Twin") `shouldBe` "Aphex Twin"

    it "shows an album's year before its name" $
      row (album "b" "Drukqs" (Just 2001)) `shouldBe` "2001  Drukqs"

    it "leaves the column blank for an album the server gave no year" $
      row (album "b" "Sketches" Nothing) `shouldBe` "      Sketches"

    it "shows a song's track number before its title" $
      row (song "s" "Vordhosbn" 293 (Just 2)) `shouldBe` "  2  Vordhosbn"

    it "leaves the column blank for a song the server gave no track number" $
      row (song "s" "Btoum Roumada" 96 Nothing) `shouldBe` "     Btoum Roumada"

  describe "draw" $ do
    it "fills the screen with the artist list under its title" $
      shown (60, 6) start
        `shouldBe` ["Artists", "anohni", "Aphex Twin", "zebra", "", ""]

    it "marks the selected row and no other" $
      highlighted theme (60, 6) (draw (after MoveDown start)) `shouldBe` ["Aphex Twin"]

    it "names the level it is on by what was descended into" $
      shown (60, 3) (after Descend (after MoveDown start))
        `shouldBe` ["Aphex Twin", "      Sketches", "1992  Selected Ambient Works 85-92"]

    it "shows the songs of the album that was descended into" $
      shown (60, 3) (after Descend (after MoveDown (after Descend (after MoveDown start))))
        `shouldBe` ["Aphex Twin — Selected Ambient Works 85-92", "  1  Xtal", "  2  Tha"]

    it "scrolls the list to keep the selection on screen" $ do
      let many = opening (map (\name -> artist name name) ["one", "two", "three", "four"])
      shown (20, 3) many `shouldBe` ["Artists", "one", "two"]
      shown (20, 3) (after MoveDown (after MoveDown many))
        `shouldBe` ["Artists", "two", "three"]

    it "keeps what went wrong in the strip along the bottom" $
      last (shown (60, 6) (stumbling Descend start))
        `shouldBe` "The server could not be reached: down"

    it "shows an empty level under an artist with no albums" $
      shown (30, 3) (after Descend (after MoveDown (after MoveDown start)))
        `shouldBe` ["zebra", "", ""]

-- | The screen a run opens on, over the stand-in library.
start :: Screen
start = opening artists

-- | The screen a command leaves behind. @q@ leaves none, and this stand-in
-- library never fails, so for those the screen it was given stands.
after :: Command -> Screen -> Screen
after = taking library

-- | The same, over a library that answers nothing.
stumbling :: Command -> Screen -> Screen
stumbling = taking (failing (NetworkFailure "down"))

taking :: Library Answer -> Command -> Screen -> Screen
taking held instruction before = fromMaybe before (left0 held instruction before)

-- | What a command left behind: nothing at all when the command was to leave
-- the player.
left :: Command -> Screen -> Maybe Screen
left = left0 library

left0 :: Library Answer -> Command -> Screen -> Maybe Screen
left0 held instruction before = runIdentity (step held instruction before)

-- | What the terminal shows, top row first, the blanks at the ends trimmed.
shown :: (Int, Int) -> Screen -> [Text]
shown region = screenshot theme region . draw
