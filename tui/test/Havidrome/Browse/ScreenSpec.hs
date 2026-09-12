-- | The browsing screen: what the keys do, what the terminal shows, what
-- picking a song does to the audio under it, and what the strip along the
-- bottom says about that audio.
--
-- The audio here is a stand-in that makes no sound, driven through a real
-- playback session, so a spec sees exactly which track the screen played and
-- when. The beat the screen runs on is struck by hand, at a moment the spec
-- names, so that nothing waits on a clock.
module Havidrome.Browse.ScreenSpec (spec) where

import Control.Monad (foldM, forM_)
import Data.Functor.Compose (Compose)
import Data.Either (fromRight)
import Data.List (transpose)
import Data.Maybe (catMaybes)
import Data.Text as T (Text)
import Data.Text qualified as T
import Graphics.Vty qualified as Vty
import Havidrome.Audio (Failure (Unplayable, Unreachable), Motion (Paused, Running))
import Havidrome.Browse (Browse (AtSongs), selected)
import Havidrome.Browse.Fixtures
  ( album
  , artist
  , artists
  , bar
  , btoumRoumada
  , drukqsSongs
  , failing
  , filledIn
  , jynweythek
  , library
  , untitled
  )
import Havidrome.Browse.Screen
  ( Command
      ( Ascend
      , Descend
      , Leave
      , LogOut
      , MoveDown
      , MoveUp
      , NextSong
      , PauseOrResume
      , PreviousSong
      , Seek
      )
  , Ending (LoggedOut, Quit)
  , Screen (browse, strip)
  , command
  , draw
  , onBeat
  , opening
  , step
  , theme
  )
import Havidrome.Browse.Row (Row (row), mark)
import Havidrome.Browse.Strip (Moment (Moment), Showing (Wrong), showing)
import Havidrome.Key (Key (..), Modifier (Ctrl, Shift))
import Havidrome.Library (Library (Library))
import Havidrome.Library qualified as Library
import Havidrome.Playback (Playing (..), Session (..))
import Havidrome.Playback.Standin
  ( Standin
  , address
  , begin
  , breakWith
  , finish
  , loaded
  , motionOf
  , reach
  , withStandin
  )
import Havidrome.Subsonic
  ( Seconds (Seconds)
  , Song (..)
  , SubsonicError (NetworkFailure)
  )
import Terminal (Cell, border, inBold, inside, reversed, runs, screenshot, terminal, vacant)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

spec :: Spec
spec = do
  keys
  stepping
  pickingASong
  browsingWhilePlaying
  leaving
  playbackKeys
  overlay
  loadingIndicator
  progressBar
  failureInStrip
  playingSymbol
  playingMark
  columns
  margin

keys :: Spec
keys = describe "command" $ do
  it "moves the selection with the arrows" $ do
    command UpArrow [] `shouldBe` Just MoveUp
    command DownArrow [] `shouldBe` Just MoveDown

  it "moves it with j and k just the same" $ do
    command (Character 'k') [] `shouldBe` Just MoveUp
    command (Character 'j') [] `shouldBe` Just MoveDown

  it "descends on Enter and returns on Esc" $ do
    command Enter [] `shouldBe` Just Descend
    command Escape [] `shouldBe` Just Ascend

  it "leaves the player on Ctrl+C" $
    command (Character 'c') [Ctrl] `shouldBe` Just Leave

  it "leaves the account on l" $
    command (Character 'l') [] `shouldBe` Just LogOut

  it "reaches the playing song with space, n and p" $ do
    command (Character ' ') [] `shouldBe` Just PauseOrResume
    command (Character 'n') [] `shouldBe` Just NextSong
    command (Character 'p') [] `shouldBe` Just PreviousSong

  it "seeks it 5s on the arrows and 30s with shift held" $ do
    command RightArrow [] `shouldBe` Just (Seek 5)
    command LeftArrow [] `shouldBe` Just (Seek (-5))
    command RightArrow [Shift] `shouldBe` Just (Seek 30)
    command LeftArrow [Shift] `shouldBe` Just (Seek (-30))

  it "ignores every other key" $ do
    command (Character 'x') [] `shouldBe` Nothing
    command Backspace [] `shouldBe` Nothing
    command (Character 'c') [] `shouldBe` Nothing

  it "is left by no letter key, q least of all" $ do
    command (Character 'q') [] `shouldBe` Nothing
    command (Character 'q') [Ctrl] `shouldBe` Nothing

stepping :: Spec
stepping = describe "step" $ do
  it "leaves the player on Ctrl+C, at any level" $ withStandin $ \_ session -> do
    ends session Leave [] `shouldReturn` Just Quit
    ends session Leave [Descend] `shouldReturn` Just Quit
    ends session Leave [Descend, Descend] `shouldReturn` Just Quit

  it "leaves the account on l, at any level" $ withStandin $ \_ session -> do
    ends session LogOut [] `shouldReturn` Just LoggedOut
    ends session LogOut [Descend] `shouldReturn` Just LoggedOut
    ends session LogOut [Descend, Descend] `shouldReturn` Just LoggedOut

  it "ends browsing on no other key" $ withStandin $ \_ session -> do
    ends session MoveDown [] `shouldReturn` Nothing
    ends session Descend [] `shouldReturn` Nothing
    ends session Ascend [Descend] `shouldReturn` Nothing

  it "descends into the level the library holds" $ withStandin $ \_ session -> do
    screen <- after session [Descend]
    onKeys screen `shouldBe` ["2016  Hopelessness"]

  it "returns to the level above, still on what was descended into" $
    withStandin $ \_ session -> do
      screen <- after session [MoveDown, Descend, Ascend]
      wide 6 screen `shouldBe` wide 6 start
      onKeys screen `shouldBe` ["Aphex Twin"]

  it "keeps the level it is on when the library will not answer" $
    withStandin $ \_ session -> do
      screen <- stumbling session [Descend]
      take 5 (wide 7 screen) `shouldBe` take 5 (wide 7 start)
      onKeys screen `shouldBe` ["anohni"]

  it "says in the strip why the library did not answer" $ withStandin $ \_ session -> do
    screen <- stumbling session [Descend]
    showing screen.strip `shouldBe` Just (Wrong "The server could not be reached: down")

  it "clears the strip on the next key press" $ withStandin $ \_ session -> do
    screen <- stumbling session [Descend]
    cleared <- taking library session screen MoveDown
    showing cleared.strip `shouldBe` Nothing

pickingASong :: Spec
pickingASong = describe "picking a song" $ do
  it "plays the song the selection is on" $ withStandin $ \standin session -> do
    _ <- after session playingDrukqs
    loaded standin `shouldReturn` [from btoumRoumada]
    playing session `shouldReturn` Just btoumRoumada

  it "leaves the lists exactly where they were, but for the mark on it" $
    withStandin $ \_ session -> do
      browsing <- after session toDrukqs
      picking <- taking library session browsing Descend
      unmarked (shown (60, 6) picking) `shouldBe` shown (60, 6) browsing
      onKeys picking `shouldBe` ["    ▶ Btoum Roumada"]

  it "then plays the rest of its album, with nothing more pressed" $
    withStandin $ \standin session -> do
      screen <- after session playingDrukqs
      _ <- ranOut standin session screen 2
      loaded standin `shouldReturn` fmap from drukqsSongs

  it "replaces what is playing when the song is of another album" $
    withStandin $ \standin session -> do
      _ <- after session (playingDrukqs <> toSketches <> [Descend])
      loaded standin
        `shouldReturn` [from btoumRoumada, from untitled]
      playing session `shouldReturn` Just untitled

  it "then follows that album to its end, and no further" $
    withStandin $ \standin session -> do
      screen <- after session (playingDrukqs <> toSketches <> [Descend])
      _ <- ranOut standin session screen 3
      loaded standin
        `shouldReturn` [from btoumRoumada, from untitled]
      session.nowPlaying `shouldReturn` Nothing

  it "plays nothing at a level that has no song to pick" $
    withStandin $ \standin session -> do
      _ <- after session [MoveDown, Descend, Descend]
      loaded standin `shouldReturn` []

  it "plays nothing in an album the library holds no songs for" $
    withStandin $ \standin session -> do
      _ <- after session [Descend, Descend, Descend]
      loaded standin `shouldReturn` []

browsingWhilePlaying :: Spec
browsingWhilePlaying = describe "browsing with a song playing" $ do
  it "goes on moving, going back up and descending, and the song plays on" $
    withStandin $ \standin session -> do
      screen <-
        resuming
          session
          playingDrukqs
          [MoveDown, MoveUp, Ascend, Ascend, MoveDown, Descend]
      trail screen `shouldBe` [["zebra"], []]
      loaded standin `shouldReturn` [from btoumRoumada]
      playing session `shouldReturn` Just btoumRoumada
      motionOf standin `shouldReturn` Just Running

  it "keeps playing that album while another artist's is browsed" $
    withStandin $ \standin session -> do
      screen <-
        resuming
          session
          playingDrukqs
          [Ascend, Ascend, MoveUp, Descend, Descend]
      _ <- ranOut standin session screen 2
      loaded standin `shouldReturn` fmap from drukqsSongs

leaving :: Spec
leaving = describe "leaving the player and leaving the account" $ do
  it "stops the audio on the way out of the player" $ withStandin $ \standin session -> do
    ends session Leave playingDrukqs `shouldReturn` Just Quit
    session.nowPlaying `shouldReturn` Nothing
    motionOf standin `shouldReturn` Nothing

  it "stops it on the way to the login screen just the same" $
    withStandin $ \standin session -> do
      ends session LogOut playingDrukqs `shouldReturn` Just LoggedOut
      session.nowPlaying `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

  it "logs out of an account with nothing playing" $
    withStandin $ \standin session -> do
      ends session LogOut toDrukqs `shouldReturn` Just LoggedOut
      loaded standin `shouldReturn` []
      session.nowPlaying `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

playbackKeys :: Spec
playbackKeys = describe "controlling the song that is playing" $ do
  holdingTheAudio
  movingSongs
  seekingKeys
  keysAndTheView

holdingTheAudio :: Spec
holdingTheAudio = do
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

movingSongs :: Spec
movingSongs = do
  it "plays the next song of the album on n" $ withStandin $ \standin session -> do
    screen <- after session playingDrukqs
    _ <- pressing session screen [NextSong]
    playing session `shouldReturn` Just jynweythek
    loaded standin `shouldReturn` fmap from (take 2 drukqsSongs)

  it "ends the playing on n on the album's last song" $
    withStandin $ \standin session -> do
      screen <- after session (toDrukqs <> [MoveDown, MoveDown, Descend])
      _ <- pressing session screen [NextSong]
      session.nowPlaying `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

  it "goes back a song on p, part-way through the one playing" $
    withStandin $ \standin session -> do
      screen <- after session (toDrukqs <> [MoveDown, Descend])
      reach standin (Seconds 60)
      _ <- pressing session screen [PreviousSong]
      playing session `shouldReturn` Just btoumRoumada
      loaded standin
        `shouldReturn` [from jynweythek, from btoumRoumada]

  it "plays the first song again on p on the first song" $
    withStandin $ \standin session -> do
      screen <- after session playingDrukqs
      reach standin (Seconds 60)
      _ <- pressing session screen [PreviousSong]
      playing session `shouldReturn` Just btoumRoumada
      loaded standin
        `shouldReturn` [from btoumRoumada, from btoumRoumada]

seekingKeys :: Spec
seekingKeys = do
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
      stepped <- pressing session screen [Seek 30]
      elapsed session `shouldReturn` Just (Seconds 96)
      loaded standin `shouldReturn` [from btoumRoumada]
      _ <- ranOut standin session stepped 1
      playing session `shouldReturn` Just jynweythek

  it "stops at the start of the track, and reaches no earlier song" $
    withStandin $ \standin session -> do
      screen <- after session playingDrukqs
      reach standin (Seconds 10)
      _ <- pressing session screen [Seek (-30)]
      elapsed session `shouldReturn` Just (Seconds 0)
      playing session `shouldReturn` Just btoumRoumada
      loaded standin `shouldReturn` [from btoumRoumada]

keysAndTheView :: Spec
keysAndTheView = do
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
      session.nowPlaying `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing
      shown (60, 6) controlled `shouldBe` shown (60, 6) browsing
overlay :: Spec
overlay = describe "the now-playing overlay" $ do
  whatTheOverlaySays
  whereTheOverlayIs

whatTheOverlaySays :: Spec
whatTheOverlaySays = do
  it "shows the playing song's name and its elapsed and total time" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      stripRow screen `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

  it "moves the elapsed time on as the audio does" $ withStandin $ \standin session -> do
    screen <- onDrukqs standin session
    reach standin (Seconds 42)
    moved <- beaten session 1 screen
    stripRow moved `shouldBe` "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"

  it "leaves the elapsed time where a held song left it" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      reach standin (Seconds 42)
      running <- beaten session 1 screen
      session.pause
      held <- beaten session 2 running >>= beaten session 3
      stripRow held `shouldBe` "⏸ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"

  it "shows the next song of the album once playback moves on" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      moved <- ranOut standin session screen 1
      stripRow moved `shouldBe` "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"

whereTheOverlayIs :: Spec
whereTheOverlayIs = do
  it "is not on screen while nothing is playing" $ withStandin $ \_ session -> do
    screen <- after session toDrukqs >>= beaten session 0
    onStrip screen `shouldBe` Nothing

  it "is there at every one of the three levels" $ withStandin $ \standin session -> do
    songs <- onDrukqs standin session
    albums <- taking library session songs Ascend >>= beaten session 1
    names <- taking library session albums Ascend >>= beaten session 2
    fmap stripRow [songs, albums, names]
      `shouldBe` replicate 3 ("⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")

  it "takes the bottom row inside the margin and the row above it, and no more of the screen" $
    withStandin $ \standin session -> do
      browsing <- after session toDrukqs
      picking <- taking library session browsing Descend
      begin standin
      started <- beaten session 0 picking
      unmarked (shown (60, 6) started)
        `shouldBe` shown (60, 4) browsing <> ["", "⏵ Btoum Roumada  " <> bar 0 30 <> "  0:00 / 1:36"]

  it "is gone when the album's last song finishes, the list left where it was" $
    withStandin $ \standin session -> do
      browsing <- after session toDrukqs
      started <- taking library session browsing Descend >>= beaten session 0
      ended <- ranOut standin session started 3
      onStrip ended `shouldBe` Nothing
      session.nowPlaying `shouldReturn` Nothing
      shown (60, 6) ended `shouldBe` shown (60, 6) browsing
loadingIndicator :: Spec
loadingIndicator = describe "a song picked with Enter, while it loads" $ do
  whileASongLoads
  loadingAndFailures

whileASongLoads :: Spec
whileASongLoads = do
  it "shows the track name and a loading indicator, and no elapsed time" $
    withStandin $ \_ session -> do
      screen <- loadingDrukqs session
      stripRow screen `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"

  it "shows the elapsed time in its place once the audio starts, moving on as it plays" $
    withStandin $ \standin session -> do
      screen <- loadingDrukqs session
      begin standin
      started <- beaten session 1 screen
      stripRow started `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
      reach standin (Seconds 42)
      moved <- beaten session 2 started
      stripRow moved `shouldBe` "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"

  it "shows it for a song picked while another still loads, until that one's audio starts" $
    withStandin $ \standin session -> do
      screen <- loadingDrukqs session
      repicked <- pressing session screen [MoveDown, Descend] >>= beaten session 1
      stripRow repicked `shouldBe` "  Jynweythek  " <> bar 0 19 <> "     ⠋ / 2:09"
      begin standin
      started <- beaten session 2 repicked
      stripRow started `shouldBe` "⏵ Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"

  it "is never shown for the song the album moves on to by itself" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      moved <- ranOut standin session screen 1
      stripRow moved `shouldBe` "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"

loadingAndFailures :: Spec
loadingAndFailures = do
  it "nor for the songs n and p move to, even away from a song still loading" $
    withStandin $ \_ session -> do
      screen <- loadingDrukqs session
      forward <- pressing session screen [NextSong] >>= beaten session 1
      stripRow forward `shouldBe` "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
      back <- pressing session forward [PreviousSong] >>= beaten session 2
      stripRow back `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

  it "finishes loading held on space, at 0:00, until space again plays it from its start" $
    withStandin $ \standin session -> do
      screen <- loadingDrukqs session
      holding <- pressing session screen [PauseOrResume] >>= beaten session 1
      stripRow holding `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"
      begin standin
      ready <- beaten session 2 holding >>= beaten session 60
      stripRow ready `shouldBe` "⏸ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
      motionOf standin `shouldReturn` Just Paused
      _ <- pressing session ready [PauseOrResume]
      motionOf standin `shouldReturn` Just Running
      elapsed session `shouldReturn` Just (Seconds 0)
      loaded standin `shouldReturn` [from btoumRoumada]

  it "gives the whole strip to a file that will not play" $
    withStandin $ \standin session -> do
      screen <- loadingDrukqs session
      breakWith standin (Unplayable "the file will not play: it is corrupt")
      failed <- beaten session 1 screen
      onStrip failed `shouldBe` Just (Wrong "The file will not play: it is corrupt")

  it "gives it to a server that cannot be reached just the same" $
    withStandin $ \standin session -> do
      screen <- loadingDrukqs session
      breakWith standin (Unreachable "the server could not be reached: it is down")
      failed <- beaten session 1 screen
      onStrip failed `shouldBe` Just (Wrong "The server could not be reached: it is down")
progressBar :: Spec
progressBar = describe "the progress bar" $ do
  howTheBarFills
  howTheBarSits

howTheBarFills :: Spec
howTheBarFills = do
  it "sits on the strip's one line, between the track name and the times" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      stripRow screen `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

  it "fills as the track plays, by the part of it that has played" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      let at seconds = reach standin (Seconds seconds) >> beaten session 1 screen
      partWay <- at 48
      stripRow partWay `shouldBe` "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
      filled <- traverse (fmap (filledIn . stripRow) . at) [6, 24, 72, 90]
      filled `shouldBe` [1, 4, 12, 15]

  it "stands still with the elapsed time while held, and moves on with it again" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      reach standin (Seconds 48)
      held <- pressing session screen [PauseOrResume] >>= beaten session 1 >>= beaten session 2
      motionOf standin `shouldReturn` Just Paused
      stripRow held `shouldBe` "⏸ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
      resumed <- pressing session held [PauseOrResume]
      motionOf standin `shouldReturn` Just Running
      reach standin (Seconds 72)
      moved <- beaten session 3 resumed
      stripRow moved `shouldBe` "⏵ Btoum Roumada  " <> bar 12 4 <> "  1:12 / 1:36"

  it "moves forward and back with a seek, as far as the elapsed time moves" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      reach standin (Seconds 48)
      let seeking sought by = pressing session sought [Seek by] >>= beaten session 1
      forward <- seeking screen 30
      stripRow forward `shouldBe` "⏵ Btoum Roumada  " <> bar 13 3 <> "  1:18 / 1:36"
      back <- seeking forward (-30)
      stripRow back `shouldBe` "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
      nudgedBack <- seeking back (-5)
      stripRow nudgedBack `shouldBe` "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:43 / 1:36"
      nudged <- seeking nudgedBack 5
      stripRow nudged `shouldBe` "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"

  it "stays at whichever end of the track a seek past it stops at" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      reach standin (Seconds 90)
      ended <- pressing session screen [Seek 30] >>= beaten session 1
      stripRow ended `shouldBe` "⏵ Btoum Roumada  " <> bar 16 0 <> "  1:36 / 1:36"
      reach standin (Seconds 10)
      started <- pressing session ended [Seek (-30)] >>= beaten session 2
      stripRow started `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

howTheBarSits :: Spec
howTheBarSits = do
  it "gives way, with the track name and the times, to a playback error" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      stripRow screen `shouldBe` "The file will not play: it is corrupt"

  it "stays empty for a track the server gives no length for, and nothing goes wrong" $
    withStandin $ \standin session -> do
      picking <- after session toSilence
      begin standin
      screen <- beaten session 0 picking
      later <- beaten session 10 screen
      fmap stripRow [screen, later]
        `shouldBe` replicate 2 ("⏵ Silence  " <> bar 0 22 <> "  0:00 / 0:00")

  it "shrinks and grows with the width of the screen" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      reach standin (Seconds 48)
      partWay <- beaten session 1 screen
      let stripAt width = drop 4 (shown (width, 5) partWay)
      stripAt 32 `shouldBe` ["⏵ Btoum Roumada  " <> bar 1 1 <> "  0:48 / 1:36"]
      stripAt 82 `shouldBe` ["⏵ Btoum Roumada  " <> bar 26 26 <> "  0:48 / 1:36"]

  it "never wraps the strip onto a second row, however narrow the screen" $
    withStandin $ \standin session -> do
      browsing <- after session toDrukqs
      picking <- taking library session browsing Descend
      begin standin
      started <- beaten session 0 picking
      unmarked (shown (20, 6) started)
        `shouldBe` shown (20, 4) browsing <> ["", "⏵ Btoum Roumada    0"]
failureInStrip :: Spec
failureInStrip = describe "a playback failure in the strip" $ do
  it "puts a skipped track's reason in place of the overlay's contents" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      onStrip screen `shouldBe` Just (Wrong "The file will not play: it is corrupt")

  it "gives the strip back to the next song's line a few seconds on" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      later <- beaten session 10 screen
      stripRow later `shouldBe` "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
      loaded standin `shouldReturn` fmap from (take 2 drukqsSongs)

  it "keeps a network failure's reason there however long it is left" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unreachable "the server could not be reached: it is down")
      waited <- beaten session 600 screen
      onStrip waited `shouldBe` Just (Wrong "The server could not be reached: it is down")
      session.nowPlaying `shouldReturn` Nothing

  it "takes it down on the next key press, which still does its usual job" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unreachable "the server could not be reached: it is down")
      moved <- taking library session screen MoveDown
      onStrip moved `shouldBe` Nothing
      onKeys moved `shouldBe` ["  1   Jynweythek"]

playingSymbol :: Spec
playingSymbol = describe "the playing or paused symbol in the strip" $ do
  whichSymbolShows
  whenNoSymbolShows

whichSymbolShows :: Spec
whichSymbolShows = do
  it "is one symbol while the audio runs and another while it is held, switched by each space" $
    withStandin $ \standin session -> do
      running <- onDrukqs standin session
      held <- pressing session running [PauseOrResume] >>= beaten session 1
      again <- pressing session held [PauseOrResume] >>= beaten session 2
      fmap standing [running, held, again] `shouldBe` [["⏵"], ["⏸"], ["⏵"]]

  it "is neither while a song picked with Enter loads, space pressed or not" $
    withStandin $ \_ session -> do
      loading <- loadingDrukqs session
      holding <- pressing session loading [PauseOrResume] >>= beaten session 1
      fmap stripRow [loading, holding]
        `shouldBe` replicate 2 ("  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36")
      fmap standing [loading, holding] `shouldBe` [[], []]

  it "is neither while a song reached by n or p, or moved on to by the album, loads" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      forward <- pressing session screen [NextSong] >>= beaten session 1
      back <- pressing session forward [PreviousSong] >>= beaten session 2
      movedOn <- ranOut standin session back 1
      fmap stripRow [forward, back, movedOn]
        `shouldBe` [ "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
                   , "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
                   , "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
                   ]
      fmap standing [forward, back, movedOn] `shouldBe` [[], [], []]

  it "is the playing one once a song loads with no space pressed, however it started" $
    withStandin $ \standin session -> do
      picked <- loadingDrukqs session
      begin standin
      pickedBegun <- beaten session 1 picked
      forward <- pressing session pickedBegun [NextSong]
      begin standin
      forwardBegun <- beaten session 2 forward
      movedOn <- ranOut standin session forwardBegun 1
      begin standin
      movedOnBegun <- beaten session 3 movedOn
      fmap standing [pickedBegun, forwardBegun, movedOnBegun] `shouldBe` replicate 3 ["⏵"]

  it "is the paused one once a song loads with space pressed while it loaded, however it started" $
    withStandin $ \standin session -> do
      picked <- loadingDrukqs session >>= flip (pressing session) [PauseOrResume]
      begin standin
      pickedBegun <- beaten session 1 picked
      forward <- pressing session pickedBegun [NextSong, PauseOrResume]
      begin standin
      forwardBegun <- beaten session 2 forward
      movedOn <- ranOut standin session forwardBegun 1 >>= flip (pressing session) [PauseOrResume]
      begin standin
      movedOnBegun <- beaten session 3 movedOn
      fmap standing [pickedBegun, forwardBegun, movedOnBegun] `shouldBe` replicate 3 ["⏸"]
      motionOf standin `shouldReturn` Just Paused

whenNoSymbolShows :: Spec
whenNoSymbolShows = do
  it "is neither while a skipped track's reason is in the strip, the next song running behind it" $
    withStandin $ \standin session -> do
      failed <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      begin standin
      behind <- beaten session 1 failed
      onStrip behind `shouldBe` Just (Wrong "The file will not play: it is corrupt")
      standing behind `shouldBe` []

  it "is neither while a network failure's reason is in the strip" $
    withStandin $ \standin session -> do
      failed <- breaking session standin (Unreachable "the server could not be reached: it is down")
      onStrip failed `shouldBe` Just (Wrong "The server could not be reached: it is down")
      standing failed `shouldBe` []

  it "is not on screen with nothing playing, and neither is the strip" $ do
    withStandin $ \_ session -> do
      opened <- after session toDrukqs >>= beaten session 0
      (onStrip opened, standing opened) `shouldBe` (Nothing, [])
    withStandin $ \standin session -> do
      finished <- onDrukqs standin session >>= \started -> ranOut standin session started 3
      (onStrip finished, standing finished) `shouldBe` (Nothing, [])
    withStandin $ \standin session -> do
      ended <-
        onDrukqs standin session
          >>= flip (pressing session) [NextSong, NextSong, NextSong]
          >>= beaten session 1
      (onStrip ended, standing ended) `shouldBe` (Nothing, [])
playingMark :: Spec
playingMark = describe "the mark on the playing song" $ do
  whereTheMarkSits
  howTheMarkMoves
  whenNothingIsMarked

whereTheMarkSits :: Spec
whereTheMarkSits = do
  it "is one space before its name, with the selection on it" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      wide 7 screen
        `shouldBe` [ across ["Artists", "Albums", "Songs"]
                   , rules 3
                   , across ["anohni", "      Sketches", "    ▶ Btoum Roumada"]
                   , across ["Aphex Twin", "1992  Selected Ambient Works 85-92", "  1   Jynweythek"]
                   , across ["zebra", "2001  Drukqs", "  2   Vordhosbn"]
                   , ""
                   , "⏵ Btoum Roumada  " <> bar 0 90 <> "  0:00 / 1:36"
                   ]
      onKeys screen `shouldBe` ["    ▶ Btoum Roumada"]

  it "stays on it with the selection moved off it, and on no other row" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session >>= flip (pressing session) [MoveDown, MoveDown]
      carrying screen `shouldBe` ["    ▶ Btoum Roumada"]
      onKeys screen `shouldBe` ["  2   Vordhosbn"]

  it "is on a picked song from Enter, while it is still loading" $
    withStandin $ \_ session -> do
      screen <- after session playingDrukqs
      carrying screen `shouldBe` ["    ▶ Btoum Roumada"]

  it "stays where it is when the song is paused" $ withStandin $ \standin session -> do
    screen <- onDrukqs standin session >>= flip (pressing session) [PauseOrResume] >>= beaten session 1
    motionOf standin `shouldReturn` Just Paused
    carrying screen `shouldBe` ["    ▶ Btoum Roumada"]

  it "is still there after Esc out of its album and Enter back into it" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session >>= flip (pressing session) [Ascend, Descend]
      songsOf screen `shouldBe` Just ("Aphex Twin", "2001  Drukqs")
      carrying screen `shouldBe` ["    ▶ Btoum Roumada"]

  it "is on no song of another album of the same artist" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session >>= flip (pressing session) toSketches
      songsOf screen `shouldBe` Just ("Aphex Twin", "      Sketches")
      carrying screen `shouldBe` []

howTheMarkMoves :: Spec
howTheMarkMoves = do
  it "is on no song of another artist's album" $ withStandin $ \standin session -> do
    screen <-
      onDrukqs standin session
        >>= flip (pressing session) [Ascend, Ascend, MoveUp, Descend, MoveDown, Descend]
    songsOf screen `shouldBe` Just ("anohni", "2017  Paradise")
    carrying screen `shouldBe` []

  it "moves on with the album when a song finishes by itself" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session >>= \started -> ranOut standin session started 1
      carrying screen `shouldBe` ["  1 ▶ Jynweythek"]

  it "moves to the next song on n" $ withStandin $ \standin session -> do
    screen <- onDrukqs standin session >>= flip (pressing session) [NextSong]
    carrying screen `shouldBe` ["  1 ▶ Jynweythek"]

  it "moves to the previous song on p" $ withStandin $ \_ session -> do
    screen <- after session (toDrukqs <> [MoveDown, Descend, PreviousSong])
    carrying screen `shouldBe` ["    ▶ Btoum Roumada"]

  it "moves past a skipped track onto the song that plays instead" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      carrying screen `shouldBe` ["  1 ▶ Jynweythek"]

  it "moves to another song picked with Enter" $ withStandin $ \standin session -> do
    screen <- onDrukqs standin session >>= flip (pressing session) [MoveDown, MoveDown, Descend]
    carrying screen `shouldBe` ["  2 ▶ Vordhosbn"]

whenNothingIsMarked :: Spec
whenNothingIsMarked = do
  it "is on no song once the album's last song has finished" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session >>= \started -> ranOut standin session started 3
      songsOf screen `shouldBe` Just ("Aphex Twin", "2001  Drukqs")
      carrying screen `shouldBe` []

  it "is on no song once n on the last song has ended the playing" $
    withStandin $ \_ session -> do
      screen <- after session (toDrukqs <> [MoveDown, MoveDown, Descend, NextSong])
      carrying screen `shouldBe` []

  it "is on no song once a network failure has stopped the playing" $
    withStandin $ \standin session -> do
      screen <- breaking session standin (Unreachable "the server could not be reached: it is down")
      carrying screen `shouldBe` []

  it "is never on an album or an artist" $ withStandin $ \standin session -> do
    albums <- onDrukqs standin session >>= flip (pressing session) [Ascend]
    names <- pressing session albums [Ascend]
    fmap carrying [albums, names] `shouldBe` [[], []]

  it "is on no song after logging out and in again" $ withStandin $ \_ session -> do
    ends session LogOut playingDrukqs `shouldReturn` Just LoggedOut
    loggedIn <- after session toDrukqs
    carrying loggedIn `shouldBe` []
columns :: Spec
columns = describe "the columns" $ do
  howColumnsOpen
  howColumnsScroll
  howColumnsAreDrawn

howColumnsOpen :: Spec
howColumnsOpen = do
  it "open on the artist list alone, one column at the left" $ do
    wide 6 start `shouldBe` ["Artists", rules 1, "anohni", "Aphex Twin", "zebra", ""]
    wide 6 start `shouldSatisfy` all ((<= 40) . T.length)

  it "put the keys on the first artist, and on no other row" $ withStandin $ \_ session -> do
    onKeys start `shouldBe` ["anohni"]
    screen <- after session [MoveDown]
    onKeys screen `shouldBe` ["Aphex Twin"]

  it "put an artist's albums in a second column, the artist highlighted in the first" $
    withStandin $ \_ session -> do
      screen <- after session [MoveDown, Descend]
      wide 6 screen
        `shouldBe` [ across ["Artists", "Albums"]
                   , rules 2
                   , across ["anohni", "      Sketches"]
                   , across ["Aphex Twin", "1992  Selected Ambient Works 85-92"]
                   , across ["zebra", "2001  Drukqs"]
                   , across ["", ""]
                   ]
      trail screen `shouldBe` [["Aphex Twin"], ["      Sketches"]]

  it "put an album's songs in a third column, the artist and the album highlighted" $
    withStandin $ \_ session -> do
      screen <- after session toDrukqs
      wide 6 screen `shouldBe` drukqsColumns
      trail screen `shouldBe` [["Aphex Twin"], ["2001  Drukqs"], ["      Btoum Roumada"]]

  it "move the keys in the rightmost column, and no highlighted row left of it" $
    withStandin $ \_ session -> do
      songs <- after session toDrukqs
      movedSongs <- pressing session songs [MoveDown, MoveDown, MoveUp]
      trail movedSongs `shouldBe` [["Aphex Twin"], ["2001  Drukqs"], ["  1   Jynweythek"]]
      albums <- after session [MoveDown, Descend]
      movedAlbums <- pressing session albums [MoveDown, MoveDown]
      trail movedAlbums `shouldBe` [["Aphex Twin"], ["2001  Drukqs"]]

howColumnsScroll :: Spec
howColumnsScroll = do
  howRowsAreDrawn
  howEscapeLeaves
  howColumnsScrollOn

howRowsAreDrawn :: Spec
howRowsAreDrawn =
  it "draw the rows picked left of the keys just as the keys' row, and none of them bold" $
    withStandin $ \_ session -> do
      albums <- after session [MoveDown, Descend]
      fmap reversed (drawnAs "      Sketches" albums) `shouldBe` [True]
      drawnAs "Aphex Twin" albums `shouldBe` drawnAs "      Sketches" albums
      emboldened albums `shouldBe` ["Artists", "Albums"]
      songs <- after session toDrukqs
      fmap reversed (drawnAs "      Btoum Roumada" songs) `shouldBe` [True]
      drawnAs "Aphex Twin" songs `shouldBe` drawnAs "      Btoum Roumada" songs
      drawnAs "2001  Drukqs" songs `shouldBe` drawnAs "      Btoum Roumada" songs
      emboldened songs `shouldBe` ["Artists", "Albums", "Songs"]

howEscapeLeaves :: Spec
howEscapeLeaves = do
  it "lose the rightmost on Esc, leaving the rest exactly as they were left" $
    withStandin $ \_ session -> do
      songs <- after session toDrukqs
      albums <- pressing session songs [Ascend]
      artistsAlone <- pressing session albums [Ascend]
      unmoved <- pressing session artistsAlone [Ascend]
      leftAtAlbums <- after session [MoveDown, Descend, MoveDown, MoveDown]
      leftAtArtists <- after session [MoveDown]
      looks albums `shouldBe` looks leftAtAlbums
      looks artistsAlone `shouldBe` looks leftAtArtists
      looks unmoved `shouldBe` looks artistsAlone
  it "give another album's songs after Esc, the albums changed only in their mark" $
    withStandin $ \_ session -> do
      screen <- after session (toDrukqs <> [Ascend, MoveUp, Descend])
      wide 6 screen `shouldBe` ambientColumns
      trail screen
        `shouldBe` [["Aphex Twin"], ["1992  Selected Ambient Works 85-92"], ["  1   Xtal"]]

howColumnsScrollOn :: Spec
howColumnsScrollOn = do
  it "scroll each on its own" $ withStandin $ \_ session -> do
    let crowded =
          Library
            { Library.artists = pure []
            , Library.albums =
                const . pure $
                  [ album "First" "First" (Just 2001)
                  , album "Second" "Second" (Just 2002)
                  , album "Third" "Third" (Just 2003)
                  , album "Fourth" "Fourth" (Just 2004)
                  , album "Fifth" "Fifth" (Just 2005)
                  ]
            , Library.songs = const (pure [])
            }
        crowding = foldM (taking crowded session)
        many = opening (fmap (\name -> artist name name) ["one", "two", "three", "four", "five", "six"])
    artistsScrolled <- crowding many [MoveDown, MoveDown, MoveDown, MoveDown]
    wide 5 artistsScrolled `shouldBe` ["Artists", rules 1, "three", "four", "five"]
    albums <- crowding artistsScrolled [Descend]
    wide 5 albums
      `shouldBe` [ across ["Artists", "Albums"]
                 , rules 2
                 , across ["three", "2001  First"]
                 , across ["four", "2002  Second"]
                 , across ["five", "2003  Third"]
                 ]
    albumsScrolled <- crowding albums [MoveDown, MoveDown, MoveDown]
    wide 5 albumsScrolled
      `shouldBe` [ across ["Artists", "Albums"]
                 , rules 2
                 , across ["three", "2002  Second"]
                 , across ["four", "2003  Third"]
                 , across ["five", "2004  Fourth"]
                 ]

  it "give an empty second column for an artist with no albums, left again on Esc" $
    withStandin $ \_ session -> do
      screen <- after session [MoveDown, MoveDown, Descend]
      wide 6 screen
        `shouldBe` [ across ["Artists", "Albums"]
                   , rules 2
                   , across ["anohni", ""]
                   , across ["Aphex Twin", ""]
                   , across ["zebra", ""]
                   , across ["", ""]
                   ]
      onKeys screen `shouldBe` []
      back <- pressing session screen [Ascend]
      leftAtArtists <- after session [MoveDown, MoveDown]
      looks back `shouldBe` looks leftAtArtists
      onKeys back `shouldBe` ["zebra"]
howColumnsAreDrawn :: Spec
howColumnsAreDrawn = do
  it "all behave the same with a song playing, which plays on" $
    withStandin $ \standin session -> do
      screen <- resuming session playingDrukqs [MoveDown, Ascend, MoveUp, Descend]
      begin standin
      caught <- beaten session 1 screen
      let strip = "⏵ Btoum Roumada  " <> bar 0 90 <> "  0:00 / 1:36"
      wide 8 caught `shouldBe` ambientColumns <> ["", strip]
      emboldened caught `shouldBe` ["Artists", "Albums", "Songs", strip]
      trail caught
        `shouldBe` [["Aphex Twin"], ["1992  Selected Ambient Works 85-92"], ["  1   Xtal"]]
      artistsAlone <- pressing session caught [Ascend, Ascend]
      take 5 (wide 7 artistsAlone) `shouldBe` take 5 (wide 7 start)
      onKeys artistsAlone `shouldBe` ["Aphex Twin"]
      playing session `shouldReturn` Just btoumRoumada
      motionOf standin `shouldReturn` Just Running

  it "shorten a row too long for its column, and wrap none onto another line" $
    withStandin $ \_ session -> do
      screen <- after session toDrukqs
      shown (24, 6) screen
        `shouldBe` [ "Artists │Albums │Songs"
                   , "────────│───────│───────"
                   , "anohni  │      …│      …"
                   , "Aphex T…│1992  …│  1   …"
                   , "zebra   │2001  …│  2   …"
                   , "        │       │"
                   ]
      let broken = opening [artist "x" "one\ntwo", artist "y" "three"]
      wide 4 broken `shouldBe` ["Artists", rules 1, "one two", "three"]

  it "have a line under the Artists heading, the first artist on the row below it" $
    take 3 (wide 6 start) `shouldBe` ["Artists", rules 1, "anohni"]

  it "have a line under each of the three headings, all on the one row" $
    withStandin $ \_ session -> do
      screen <- after session toDrukqs
      take 3 (wide 6 screen)
        `shouldBe` [ across ["Artists", "Albums", "Songs"]
                   , rules 3
                   , across ["anohni", "      Sketches", "      Btoum Roumada"]
                   ]

  it "draw the rule between two columns on the line's row as on every other row" $
    withStandin $ \_ session -> do
      screen <- after session toDrukqs
      let rule at = do
            let drawn = downColumn 6 at screen
            fmap snd drawn `shouldBe` replicate 6 '│'
            -- Every cell of the rule is drawn as the first of them is.
            drawn `shouldBe` concat (replicate 6 (take 1 drawn))
      rule 40
      rule 80

  it "keep what went wrong in the strip along the bottom" $ withStandin $ \_ session -> do
    screen <- stumbling session [Descend]
    drop 5 (wide 6 screen) `shouldBe` ["The server could not be reached: down"]
margin :: Spec
margin = describe "the margin" $ do
  it "leaves the terminal's outer rows and columns blank at every level, the columns inside" $
    withStandin $ \_ session -> do
      albums <- after session [MoveDown, Descend]
      songs <- after session toDrukqs
      forM_ [start, albums, songs] $ \screen ->
        border (whole (122, 8) screen) `shouldSatisfy` all vacant
      screenshot (whole (122, 8) songs) `shouldBe` [""] <> fmap (" " <>) drukqsColumns <> [""]

  it "has a song's strip on the row above its blank bottom row, and a blank row above the strip" $
    withStandin $ \standin session -> do
      screen <- onDrukqs standin session
      let rows = whole (46, 8) screen
      border rows `shouldSatisfy` all vacant
      drop 5 (screenshot rows) `shouldBe` ["", " ⏵ Btoum Roumada  " <> bar 0 14 <> "  0:00 / 1:36", ""]
      take 1 (drop 5 rows) `shouldSatisfy` all (all vacant)

  it "has an error in the strip in just the same place, with the same blank rows" $
    withStandin $ \standin session -> do
      failed <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      unanswered <- stumbling session [Descend]
      forM_ [(failed, "The file will not play: it is corrupt"), (unanswered, "The server could not be reached: down")] $
        \(screen, said) -> do
          let rows = whole (46, 8) screen
          border rows `shouldSatisfy` all vacant
          drop 5 (screenshot rows) `shouldBe` ["", " " <> said, ""]
          take 1 (drop 5 rows) `shouldSatisfy` all (all vacant)

  it "stays blank whatever the terminal's width and height" $
    withStandin $ \standin session -> do
      albums <- after session [MoveDown, Descend]
      loading <- loadingDrukqs session
      playingScreen <- onDrukqs standin session
      failed <- breaking session standin (Unplayable "the file will not play: it is corrupt")
      forM_ [start, albums, loading, playingScreen, failed] $ \screen ->
        forM_ sizes $ \region ->
          (region, filter (not . vacant) (border (whole region screen))) `shouldBe` (region, [])

-- | The screen a run opens on, over the stand-in library.
start :: Screen
start = opening artists

-- | The keys that walk from the artist list to the songs of Drukqs, whose
-- first song is then the one Enter picks.
toDrukqs :: [Command]
toDrukqs = [MoveDown, Descend, MoveDown, MoveDown, Descend]

-- | The keys that walk to the last song of Selected Ambient Works, which the
-- server gives no length for, and start it.
toSilence :: [Command]
toSilence = [MoveDown, Descend, MoveDown, Descend, MoveDown, MoveDown, Descend]

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

walking :: Library (Compose IO (Either SubsonicError)) -> Session -> [Command] -> IO Screen
walking held session = foldM (taking held session) start

-- | The screen one key press leaves behind. A key that ends browsing leaves
-- none, and for that the screen it was pressed on stands.
taking :: Library (Compose IO (Either SubsonicError)) -> Session -> Screen -> Command -> IO Screen
taking held session screen instruction =
  fromRight screen <$> step held session instruction screen

-- | How this key ends browsing, pressed after those ones — and nothing at all
-- when it leaves browsing going on.
ends :: Session -> Command -> [Command] -> IO (Maybe Ending)
ends session instruction path = do
  screen <- after session path
  either Just (const Nothing) <$> step library session instruction screen

-- | The song the session is playing, if it is playing one.
playing :: Session -> IO (Maybe Song)
playing session = fmap (fmap (.song)) session.nowPlaying

-- | How far into that song the audio has come.
elapsed :: Session -> IO (Maybe Seconds)
elapsed session = fmap (fmap (.elapsed)) session.nowPlaying

-- | The audio runs out, so many times, with nothing pressed: only the beat the
-- screen takes it in on. This is the whole of \"playback continues through the
-- album\".
ranOut :: Standin -> Session -> Screen -> Int -> IO Screen
ranOut standin session screen times =
  foldM (\sofar _ -> finish standin >> beaten session 0 sofar) screen [1 .. times]

-- | The screen one beat leaves behind, struck at this moment on the player's
-- clock. Every spec here strikes its own beats, so none of them waits.
beaten :: Session -> Double -> Screen -> IO Screen
beaten session at = onBeat session (Moment at)

-- | The screen with the first song of Drukqs picked, its audio started, and
-- the strip caught up with it, which is where every spec about the overlay
-- starts.
onDrukqs :: Standin -> Session -> IO Screen
onDrukqs standin session = do
  picking <- after session playingDrukqs
  begin standin
  beaten session 0 picking

-- | The screen with the first song of Drukqs just picked, its audio not yet
-- started, and the strip caught up with it.
loadingDrukqs :: Session -> IO Screen
loadingDrukqs session = after session playingDrukqs >>= beaten session 0

-- | The screen the first song of Drukqs failing this way leaves behind: the
-- backend says so, and the next beat takes it in.
breaking :: Session -> Standin -> Failure -> IO Screen
breaking session standin failure = do
  screen <- onDrukqs standin session
  breakWith standin failure
  beaten session 0 screen

-- | What the strip along the bottom has on it.
onStrip :: Screen -> Maybe Showing
onStrip screen = showing screen.strip

-- | The strip's row as a terminal 46 columns wide shows it: wide enough to
-- give the first song of Drukqs a bar of 16 columns, one for every 6s of its
-- 1:36.
stripRow :: Screen -> Text
stripRow = mconcat . drop 4 . shown (46, 5)

-- | Which of the strip's two symbols, the playing one and the paused one, are
-- anywhere on a terminal wide and tall enough to show every column and the
-- strip.
standing :: Screen -> [Text]
standing screen = filter (\symbol -> any (T.isInfixOf symbol) (wide 8 screen)) ["⏵", "⏸"]

-- | What the backend is told when that song is played from its beginning.
from :: Song -> (Text, Seconds)
from picked = (address picked.id, Seconds 0)

-- | Every cell of a terminal of this size.
whole :: (Int, Int) -> Screen -> [[Cell]]
whole region = terminal theme region . draw

-- | Every cell inside the margin of a terminal with this many columns and rows
-- inside it.
within :: (Int, Int) -> Screen -> [[Cell]]
within (width, height) = inside . whole (width + 2, height + 2)

-- | What that terminal shows inside its margin, top row first, the blanks at
-- the ends trimmed.
shown :: (Int, Int) -> Screen -> [Text]
shown region = screenshot . within region

-- | Terminals from none at all, through ones too small to hold anything inside
-- their margin, to ones that hold every column and the strip.
sizes :: [(Int, Int)]
sizes = [(width, height) | width <- [0 .. 6] <> [24, 46, 122], height <- [0 .. 6] <> [8, 24]]

-- | What a terminal with 120 columns and this many rows inside its margin
-- shows: 40 columns to each level, which no row of the stand-in library
-- outgrows.
wide :: Int -> Screen -> [Text]
wide height = shown (120, height)

-- | A row of that terminal as the columns it crosses read, left to right.
-- Each column takes 40 of its columns, the second and third starting with the
-- rule between them and the column to the left.
across :: [Text] -> Text
across = T.stripEnd . T.intercalate "│" . zipWith (`T.justifyLeft` ' ') (40 : repeat 39)

-- | The row of that terminal under the headings of this many columns: a line
-- across each column, and the rule between one column and the next as it is on
-- every other row.
rules :: Int -> Text
rules count = T.intercalate "│" (fmap (`T.replicate` "─") (take count (40 : repeat 39)))

-- | What that terminal has in this one of its columns, top row to bottom, and
-- how each is drawn: the look of the rule between two columns, taken whole.
downColumn :: Int -> Int -> Screen -> [Cell]
downColumn height at = concatMap (take 1 . drop at) . within (120, height)

-- | Where the keys are on that terminal: the row highlighted in its rightmost
-- column.
onKeys :: Screen -> [Text]
onKeys = foldl' (\_ rightmost -> rightmost) [] . trail

-- | The rows highlighted on that terminal, column by column from the artists
-- rightwards, each column's top to bottom: the row picked in each column left
-- of the one being browsed, and the row the keys are on in that one.
trail :: Screen -> [[Text]]
trail = fmap catMaybes . transpose . fmap (fmap highlit . cells . concatMap letters) . runs . within (120, 6)
  where
    letters (look, said) = fmap (reversed look,) (T.unpack said)
    cells characters = case break ((== '│') . snd) characters of
      (cell, []) -> [cell]
      (cell, _ : rest) -> cell : cells rest
    highlit cell = case [character | (True, character) <- cell] of
      [] -> Nothing
      lit -> Just (T.stripEnd (T.pack lit))

-- | How each run of that terminal that reads exactly this is drawn.
drawnAs :: Text -> Screen -> [Maybe Vty.Attr]
drawnAs said = fmap fst . concatMap (filter ((== said) . T.stripEnd . snd)) . runs . within (120, 6)

-- | Each stretch of it in bold: the columns' headings and the now-playing
-- overlay.
emboldened :: Screen -> [Text]
emboldened = inBold . within (120, 6)

-- | Everything a look at that terminal takes in: what it says, the rows
-- highlighted in each column, and what stands out in bold.
looks :: Screen -> ([Text], [[Text]], [Text])
looks screen = (wide 6 screen, trail screen, emboldened screen)

-- | The three columns down to the songs of Drukqs, six rows high.
drukqsColumns :: [Text]
drukqsColumns =
  [ across ["Artists", "Albums", "Songs"]
  , rules 3
  , across ["anohni", "      Sketches", "      Btoum Roumada"]
  , across ["Aphex Twin", "1992  Selected Ambient Works 85-92", "  1   Jynweythek"]
  , across ["zebra", "2001  Drukqs", "  2   Vordhosbn"]
  , across ["", "", ""]
  ]

-- | The three columns down to the songs of Selected Ambient Works 85-92, six
-- rows high.
ambientColumns :: [Text]
ambientColumns =
  [ across ["Artists", "Albums", "Songs"]
  , rules 3
  , across ["anohni", "      Sketches", "  1   Xtal"]
  , across ["Aphex Twin", "1992  Selected Ambient Works 85-92", "  2   Tha"]
  , across ["zebra", "2001  Drukqs", "  3   Silence"]
  , across ["", "", ""]
  ]

-- | The rows that carry the playing mark, as the column each is in reads them,
-- on a terminal wide enough to leave every row whole.
carrying :: Screen -> [Text]
carrying = concatMap (filter (T.isInfixOf mark) . fmap T.stripEnd . T.splitOn "│") . wide 8

-- | The same rows with the mark given back the space it stands in, which is
-- what they read as with nothing playing.
unmarked :: [Text] -> [Text]
unmarked = fmap (T.replace mark " ")

-- | The artist and the album whose songs are the column being browsed, as
-- their rows read, when songs are what is being browsed.
songsOf :: Screen -> Maybe (Text, Text)
songsOf screen = case screen.browse of
  AtSongs names records _ -> (,) . row <$> selected names <*> (row <$> selected records)
  _ -> Nothing
