{-# LANGUAGE OverloadedStrings #-}

-- | The browsing screen: what the keys do, what the terminal shows, and what
-- picking a song does to the audio under it.
--
-- The audio here is a stand-in that makes no sound, driven through a real
-- playback session, so a spec sees exactly which track the screen played and
-- when.
module Havidrome.Browse.ScreenSpec (spec) where

import Control.Monad (foldM)
import Control.Monad.Trans.Except (ExceptT)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Graphics.Vty qualified as Vty
import Havidrome.Audio (Motion (Running))
import Havidrome.Browse.Fixtures
  ( album
  , artist
  , artists
  , drukqsSongs
  , failing
  , library
  , sketchesSongs
  , song
  )
import Havidrome.Browse.Screen
  ( Command (Ascend, Descend, MoveDown, MoveUp, Quit)
  , Screen (browse, trouble)
  , command
  , draw
  , onBeat
  , opening
  , row
  , step
  , theme
  , title
  )
import Havidrome.Library (Library)
import Havidrome.Playback (Playing (playingSong), Session, nowPlaying)
import Havidrome.Playback.Standin
  ( Standin
  , address
  , finish
  , loaded
  , motionOf
  , withStandin
  )
import Havidrome.Subsonic
  ( Seconds (Seconds)
  , Song (songId)
  , SubsonicError (NetworkFailure)
  )
import Terminal (highlighted, screenshot)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

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
    it "leaves the player on q, at any level" $ withStandin $ \_ session -> do
      quitting session [] >>= (`shouldSatisfy` isNothing)
      quitting session [Descend] >>= (`shouldSatisfy` isNothing)
      quitting session [Descend, Descend] >>= (`shouldSatisfy` isNothing)

    it "descends into the level the library holds" $ withStandin $ \_ session -> do
      screen <- after session [Descend]
      title (browse screen) `shouldBe` "anohni"

    it "returns to the level above, still on what was descended into" $
      withStandin $ \_ session -> do
        screen <- after session [MoveDown, Descend, Ascend]
        title (browse screen) `shouldBe` "Artists"

    it "keeps the level it is on when the library will not answer" $
      withStandin $ \_ session -> do
        screen <- stumbling session [Descend]
        title (browse screen) `shouldBe` "Artists"

    it "says in the strip why the library did not answer" $ withStandin $ \_ session -> do
      screen <- stumbling session [Descend]
      trouble screen `shouldBe` Just "The server could not be reached: down"

    it "clears the strip on the next key press" $ withStandin $ \_ session -> do
      screen <- stumbling session [Descend]
      cleared <- taking library session screen MoveDown
      trouble cleared `shouldBe` Nothing

  describe "picking a song" $ do
    it "plays the song the selection is on" $ withStandin $ \standin session -> do
      _ <- after session (toDrukqs <> [Descend])
      loaded standin `shouldReturn` [from (drukqsSongs !! 0)]
      playing session `shouldReturn` Just (drukqsSongs !! 0)

    it "leaves the lists exactly where they were" $ withStandin $ \_ session -> do
      browsing <- after session toDrukqs
      picking <- taking library session browsing Descend
      shown (60, 6) picking `shouldBe` shown (60, 6) browsing

    it "then plays the rest of its album, with nothing more pressed" $
      withStandin $ \standin session -> do
        _ <- after session (toDrukqs <> [Descend])
        ranOut standin session 2
        loaded standin `shouldReturn` map from drukqsSongs

    it "replaces what is playing when the song is of another album" $
      withStandin $ \standin session -> do
        _ <- after session (toDrukqs <> [Descend] <> toSketches <> [Descend])
        loaded standin
          `shouldReturn` [from (drukqsSongs !! 0), from (sketchesSongs !! 0)]
        playing session `shouldReturn` Just (sketchesSongs !! 0)

    it "then follows that album to its end, and no further" $
      withStandin $ \standin session -> do
        _ <- after session (toDrukqs <> [Descend] <> toSketches <> [Descend])
        ranOut standin session 3
        loaded standin
          `shouldReturn` [from (drukqsSongs !! 0), from (sketchesSongs !! 0)]
        nowPlaying session `shouldReturn` Nothing

    it "plays nothing at a level that has no song to pick" $
      withStandin $ \standin session -> do
        _ <- after session [MoveDown, Descend, Descend]
        loaded standin `shouldReturn` []

    it "plays nothing in an album the library holds no songs for" $
      withStandin $ \standin session -> do
        _ <- after session [Descend, Descend, Descend]
        loaded standin `shouldReturn` []

  describe "browsing with a song playing" $ do
    it "goes on moving, going back up and descending, and the song plays on" $
      withStandin $ \standin session -> do
        screen <-
          resuming
            session
            (toDrukqs <> [Descend])
            [MoveDown, MoveUp, Ascend, Ascend, MoveDown, Descend]
        title (browse screen) `shouldBe` "zebra"
        loaded standin `shouldReturn` [from (drukqsSongs !! 0)]
        playing session `shouldReturn` Just (drukqsSongs !! 0)
        motionOf standin `shouldReturn` Just Running

    it "keeps playing that album while another artist's is browsed" $
      withStandin $ \standin session -> do
        _ <-
          resuming
            session
            (toDrukqs <> [Descend])
            [Ascend, Ascend, MoveUp, Descend, Descend]
        ranOut standin session 2
        loaded standin `shouldReturn` map from drukqsSongs

  describe "q with a song playing" $
    it "stops the audio on the way out of the player" $ withStandin $ \standin session -> do
      left <- quitting session (toDrukqs <> [Descend])
      left `shouldSatisfy` isNothing
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

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
      shown (60, 6) start `shouldBe` ["Artists", "anohni", "Aphex Twin", "zebra", "", ""]

    it "marks the selected row and no other" $ withStandin $ \_ session -> do
      screen <- after session [MoveDown]
      highlighted theme (60, 6) (draw screen) `shouldBe` ["Aphex Twin"]

    it "names the level it is on by what was descended into" $
      withStandin $ \_ session -> do
        screen <- after session [MoveDown, Descend]
        shown (60, 3) screen
          `shouldBe` ["Aphex Twin", "      Sketches", "1992  Selected Ambient Works 85-92"]

    it "shows the songs of the album that was descended into" $
      withStandin $ \_ session -> do
        screen <- after session [MoveDown, Descend, MoveDown, Descend]
        shown (60, 3) screen
          `shouldBe` ["Aphex Twin — Selected Ambient Works 85-92", "  1  Xtal", "  2  Tha"]

    it "scrolls the list to keep the selection on screen" $ withStandin $ \_ session -> do
      let many = opening (map (\name -> artist name name) ["one", "two", "three", "four"])
      shown (20, 3) many `shouldBe` ["Artists", "one", "two"]
      scrolled <- foldM (taking library session) many [MoveDown, MoveDown]
      shown (20, 3) scrolled `shouldBe` ["Artists", "two", "three"]

    it "keeps what went wrong in the strip along the bottom" $ withStandin $ \_ session -> do
      screen <- stumbling session [Descend]
      last (shown (60, 6) screen) `shouldBe` "The server could not be reached: down"

    it "shows an empty level under an artist with no albums" $ withStandin $ \_ session -> do
      screen <- after session [MoveDown, MoveDown, Descend]
      shown (30, 3) screen `shouldBe` ["zebra", "", ""]

-- | The screen a run opens on, over the stand-in library.
start :: Screen
start = opening artists

-- | The keys that walk from the artist list to the songs of Drukqs, whose
-- first song is then the one Enter picks.
toDrukqs :: [Command]
toDrukqs = [MoveDown, Descend, MoveDown, MoveDown, Descend]

-- | The keys that walk from the songs of Drukqs to the songs of Sketches, the
-- other album of the same artist.
toSketches :: [Command]
toSketches = [Ascend, MoveUp, MoveUp, Descend]

-- | The screen these key presses leave behind, from the one a run opens on.
after :: Session -> [Command] -> IO Screen
after = walking library

-- | The same, over a library that answers nothing.
stumbling :: Session -> [Command] -> IO Screen
stumbling = walking (failing (NetworkFailure "down"))

-- | The same again, from a screen these key presses have already been walked
-- to: what a run does next, with whatever they started still playing.
resuming :: Session -> [Command] -> [Command] -> IO Screen
resuming session already next = do
  screen <- after session already
  foldM (taking library session) screen next

walking :: Library (ExceptT SubsonicError IO) -> Session -> [Command] -> IO Screen
walking held session = foldM (taking held session) start

-- | The screen one key press leaves behind. @q@ leaves none, and for that the
-- screen it was pressed on stands.
taking :: Library (ExceptT SubsonicError IO) -> Session -> Screen -> Command -> IO Screen
taking held session screen instruction =
  fromMaybe screen <$> step held session instruction screen

-- | What @q@ leaves behind after these key presses: nothing at all.
quitting :: Session -> [Command] -> IO (Maybe Screen)
quitting session path = do
  screen <- after session path
  step library session Quit screen

-- | The song the session is playing, if it is playing one.
playing :: Session -> IO (Maybe Song)
playing = fmap (fmap playingSong) . nowPlaying

-- | The audio runs out, so many times, with nothing pressed: only the beat the
-- screen takes it in on. This is the whole of \"playback continues through the
-- album\".
ranOut :: Standin -> Session -> Int -> IO ()
ranOut standin session times =
  mapM_ (const (finish standin >> onBeat session)) [1 .. times]

-- | What the backend is told when that song is played from its beginning.
from :: Song -> (Text, Seconds)
from picked = (address (songId picked), Seconds 0)

-- | What the terminal shows, top row first, the blanks at the ends trimmed.
shown :: (Int, Int) -> Screen -> [Text]
shown region = screenshot theme region . draw
