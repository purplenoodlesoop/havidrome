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
import Havidrome.Audio (Motion (Paused, Running))
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
  ( Command
      ( Ascend
      , Descend
      , MoveDown
      , MoveUp
      , NextSong
      , PauseOrResume
      , PreviousSong
      , Quit
      , Seek
      )
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
import Havidrome.Playback (Playing (playingElapsed, playingSong), Session, nowPlaying)
import Havidrome.Playback.Standin
  ( Standin
  , address
  , finish
  , loaded
  , motionOf
  , reach
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

    it "quits on Ctrl+C" $
      command (Vty.KChar 'c') [Vty.MCtrl] `shouldBe` Just Quit

    it "reaches the playing song with space, n and p" $ do
      command (Vty.KChar ' ') [] `shouldBe` Just PauseOrResume
      command (Vty.KChar 'n') [] `shouldBe` Just NextSong
      command (Vty.KChar 'p') [] `shouldBe` Just PreviousSong

    it "seeks it 5s on the arrows and 30s with shift held" $ do
      command Vty.KRight [] `shouldBe` Just (Seek 5)
      command Vty.KLeft [] `shouldBe` Just (Seek (-5))
      command Vty.KRight [Vty.MShift] `shouldBe` Just (Seek 30)
      command Vty.KLeft [Vty.MShift] `shouldBe` Just (Seek (-30))

    it "ignores every other key" $ do
      command (Vty.KChar 'x') [] `shouldBe` Nothing
      command Vty.KBS [] `shouldBe` Nothing
      command (Vty.KChar 'c') [] `shouldBe` Nothing

    it "is left by no letter key, q least of all" $ do
      command (Vty.KChar 'q') [] `shouldBe` Nothing
      command (Vty.KChar 'q') [Vty.MCtrl] `shouldBe` Nothing

  describe "step" $ do
    it "leaves the player on Ctrl+C, at any level" $ withStandin $ \_ session -> do
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
      _ <- after session playingDrukqs
      loaded standin `shouldReturn` [from (drukqsSongs !! 0)]
      playing session `shouldReturn` Just (drukqsSongs !! 0)

    it "leaves the lists exactly where they were" $ withStandin $ \_ session -> do
      browsing <- after session toDrukqs
      picking <- taking library session browsing Descend
      shown (60, 6) picking `shouldBe` shown (60, 6) browsing

    it "then plays the rest of its album, with nothing more pressed" $
      withStandin $ \standin session -> do
        _ <- after session playingDrukqs
        ranOut standin session 2
        loaded standin `shouldReturn` map from drukqsSongs

    it "replaces what is playing when the song is of another album" $
      withStandin $ \standin session -> do
        _ <- after session (playingDrukqs <> toSketches <> [Descend])
        loaded standin
          `shouldReturn` [from (drukqsSongs !! 0), from (sketchesSongs !! 0)]
        playing session `shouldReturn` Just (sketchesSongs !! 0)

    it "then follows that album to its end, and no further" $
      withStandin $ \standin session -> do
        _ <- after session (playingDrukqs <> toSketches <> [Descend])
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
            playingDrukqs
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
            playingDrukqs
            [Ascend, Ascend, MoveUp, Descend, Descend]
        ranOut standin session 2
        loaded standin `shouldReturn` map from drukqsSongs

  describe "Ctrl+C with a song playing" $
    it "stops the audio on the way out of the player" $ withStandin $ \standin session -> do
      left <- quitting session playingDrukqs
      left `shouldSatisfy` isNothing
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

  describe "controlling the song that is playing" $ do
    it "holds the audio on space, and freezes the elapsed time with it" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 40)
        _ <- pressing session screen [PauseOrResume]
        motionOf standin `shouldReturn` Just Paused
        elapsed session `shouldReturn` Just (Seconds 40)

    it "lets it run on from the same point when space is pressed again" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 40)
        _ <- pressing session screen [PauseOrResume, PauseOrResume]
        motionOf standin `shouldReturn` Just Running
        elapsed session `shouldReturn` Just (Seconds 40)

    it "plays the next song of the album on n" $ withStandin $ \standin session -> do
      screen <- after session playingDrukqs
      _ <- pressing session screen [NextSong]
      playing session `shouldReturn` Just (drukqsSongs !! 1)
      loaded standin `shouldReturn` map from (take 2 drukqsSongs)

    it "ends the playing on n on the album's last song" $
      withStandin $ \standin session -> do
        screen <- after session (toDrukqs <> [MoveDown, MoveDown, Descend])
        _ <- pressing session screen [NextSong]
        nowPlaying session `shouldReturn` Nothing
        motionOf standin `shouldReturn` Nothing

    it "goes back a song on p, part-way through the one playing" $
      withStandin $ \standin session -> do
        screen <- after session (toDrukqs <> [MoveDown, Descend])
        reach standin (Seconds 60)
        _ <- pressing session screen [PreviousSong]
        playing session `shouldReturn` Just (drukqsSongs !! 0)
        loaded standin
          `shouldReturn` [from (drukqsSongs !! 1), from (drukqsSongs !! 0)]

    it "plays the first song again on p on the first song" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 60)
        _ <- pressing session screen [PreviousSong]
        playing session `shouldReturn` Just (drukqsSongs !! 0)
        loaded standin
          `shouldReturn` [from (drukqsSongs !! 0), from (drukqsSongs !! 0)]

    it "moves the song 5s on the arrows, and the elapsed time with it" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 40)
        _ <- pressing session screen [Seek 5]
        elapsed session `shouldReturn` Just (Seconds 45)
        _ <- pressing session screen [Seek (-5)]
        elapsed session `shouldReturn` Just (Seconds 40)

    it "moves it 30s with shift held" $ withStandin $ \standin session -> do
      screen <- after session playingDrukqs
      reach standin (Seconds 40)
      _ <- pressing session screen [Seek 30]
      elapsed session `shouldReturn` Just (Seconds 70)
      _ <- pressing session screen [Seek (-30)]
      elapsed session `shouldReturn` Just (Seconds 40)

    it "stops at the end of the track, which then finishes into the next song" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 90)
        _ <- pressing session screen [Seek 30]
        elapsed session `shouldReturn` Just (Seconds 96)
        loaded standin `shouldReturn` [from (drukqsSongs !! 0)]
        ranOut standin session 1
        playing session `shouldReturn` Just (drukqsSongs !! 1)

    it "stops at the start of the track, and reaches no earlier song" $
      withStandin $ \standin session -> do
        screen <- after session playingDrukqs
        reach standin (Seconds 10)
        _ <- pressing session screen [Seek (-30)]
        elapsed session `shouldReturn` Just (Seconds 0)
        playing session `shouldReturn` Just (drukqsSongs !! 0)
        loaded standin `shouldReturn` [from (drukqsSongs !! 0)]

    it "leaves the selection and the level as they were, at every level" $
      withStandin $ \_ session -> do
        let unmoved path = do
              browsing <- after session (playingDrukqs <> path)
              controlled <- pressing session browsing controls
              shown (60, 6) controlled `shouldBe` shown (60, 6) browsing
        unmoved []
        unmoved [Ascend]
        unmoved [Ascend, Ascend]

    it "does nothing at all with nothing playing" $
      withStandin $ \standin session -> do
        browsing <- after session toDrukqs
        controlled <- pressing session browsing controls
        loaded standin `shouldReturn` []
        nowPlaying session `shouldReturn` Nothing
        motionOf standin `shouldReturn` Nothing
        shown (60, 6) controlled `shouldBe` shown (60, 6) browsing

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

-- | The keys that walk to the songs of Drukqs and start the first of them, so
-- that the controls have a song to reach.
playingDrukqs :: [Command]
playingDrukqs = toDrukqs <> [Descend]

-- | Every control the playing song has over it, pressed one after another: a
-- hold and a release, a song forward and a song back, a nudge and a stride.
controls :: [Command]
controls =
  [PauseOrResume, PauseOrResume, NextSong, PreviousSong, Seek 5, Seek (-30)]

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

-- | The screen these key presses leave behind, pressed on one already walked
-- to: what a run does next, with whatever it started still playing.
pressing :: Session -> Screen -> [Command] -> IO Screen
pressing session = foldM (taking library session)

-- | The same, from the screen the first lot of key presses walk to.
resuming :: Session -> [Command] -> [Command] -> IO Screen
resuming session already next = do
  screen <- after session already
  pressing session screen next

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

-- | How far into that song the audio has come.
elapsed :: Session -> IO (Maybe Seconds)
elapsed = fmap (fmap playingElapsed) . nowPlaying

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
