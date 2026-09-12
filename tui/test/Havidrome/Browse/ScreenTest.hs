-- | The browsing screen: what the keys do, what the terminal shows, what
-- picking a song does to the audio under it, and what the strip along the
-- bottom says about that audio.
--
-- The audio here is a stand-in that makes no sound, driven through a real
-- playback session, so a test sees exactly which track the screen played and
-- when. The beat the screen runs on is struck by hand, at a moment the test
-- names, so that nothing waits on a clock.
module Havidrome.Browse.ScreenTest (tests) where

import Control.Monad (foldM, forM)
import Data.Either (fromRight)
import Data.List (group, transpose)
import Data.Maybe (catMaybes, isNothing)
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
import Havidrome.Browse.Row (Row (row), mark)
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
import Havidrome.Browse.Strip (Moment (Moment), Showing (Wrong), showing)
import Havidrome.Check (Checks, example)
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
import Hedgehog
  ( Gen
  , Group (Group)
  , PropertyT
  , assert
  , evalIO
  , forAll
  , property
  , (===)
  , (/==)
  )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Terminal (Cell, border, inBold, inside, reversed, runs, screenshot, terminal, vacant)

tests :: Group
tests =
  Group
    "Havidrome.Browse.Screen"
    ( keys
        <> ignoring
        <> stepping
        <> picking
        <> silent
        <> browsingOn
        <> leaving
        <> pausing
        <> skipping
        <> seeks
        <> leftAlone
        <> overlay
        <> placing
        <> indicator
        <> noIndicator
        <> complaints
        <> progress
        <> limits
        <> reasons
        <> eitherSymbol
        <> onceLoaded
        <> noSymbol
        <> marking
        <> elsewhere
        <> moving
        <> gone
        <> columns
        <> rowPicked
        <> returning
        <> scrolling
        <> alongside
        <> shortened
        <> headings
        <> margin
    )

-- | What each key press the screen binds means to it.
keys :: Checks
keys =
  [
    ( "command moves the selection with the arrows"
    , example do
        command UpArrow [] === Just MoveUp
        command DownArrow [] === Just MoveDown
    )
  ,
    ( "command moves it with j and k just the same"
    , example do
        command (Character 'k') [] === Just MoveUp
        command (Character 'j') [] === Just MoveDown
    )
  ,
    ( "command descends on Enter and returns on Esc"
    , example do
        command Enter [] === Just Descend
        command Escape [] === Just Ascend
    )
  ,
    ( "command leaves the player on Ctrl+C"
    , example (command (Character 'c') [Ctrl] === Just Leave)
    )
  ,
    ( "command leaves the account on l"
    , example (command (Character 'l') [] === Just LogOut)
    )
  ,
    ( "command reaches the playing song with space, n and p"
    , example do
        command (Character ' ') [] === Just PauseOrResume
        command (Character 'n') [] === Just NextSong
        command (Character 'p') [] === Just PreviousSong
    )
  ,
    ( "command seeks it 5s on the arrows and 30s with shift held"
    , example do
        command RightArrow [] === Just (Seek 5)
        command LeftArrow [] === Just (Seek (-5))
        command RightArrow [Shift] === Just (Seek 30)
        command LeftArrow [Shift] === Just (Seek (-30))
    )
  ]

-- | The key presses the screen binds to nothing at all.
ignoring :: Checks
ignoring =
  [
    ( "command ignores every other key"
    , example do
        command (Character 'x') [] === Nothing
        command Backspace [] === Nothing
        command (Character 'c') [] === Nothing
    )
  ,
    ( "command is left by no letter key, q least of all"
    , example do
        command (Character 'q') [] === Nothing
        command (Character 'q') [Ctrl] === Nothing
    )
  ,
    ( "command ignores every key press the screen does not bind, whatever is held with it"
    , property do
        press <- forAll unbound
        uncurry command press === Nothing
    )
  ,
    ( "command binds no letter key on its own to leaving the player"
    , property do
        letter <- forAll Gen.alpha
        command (Character letter) [] /== Just Leave
    )
  ]

-- | What one key press does to the screen and to the run.
stepping :: Checks
stepping =
  [
    ( "step leaves the player on Ctrl+C, at any level"
    , example do
        ended <- driving $ \_ session ->
          forM [[], [Descend], [Descend, Descend]] (ends session Leave)
        ended === replicate 3 (Just Quit)
    )
  ,
    ( "step leaves the account on l, at any level"
    , example do
        ended <- driving $ \_ session ->
          forM [[], [Descend], [Descend, Descend]] (ends session LogOut)
        ended === replicate 3 (Just LoggedOut)
    )
  ,
    ( "step ends browsing on no other key"
    , example do
        ended <- driving $ \_ session ->
          forM [(MoveDown, []), (Descend, []), (Ascend, [Descend])] (uncurry (ends session))
        ended === replicate 3 Nothing
    )
  ,
    ( "step descends into the level the library holds"
    , example do
        screen <- driving (\_ session -> after session [Descend])
        onKeys screen === ["2016  Hopelessness"]
    )
  ,
    ( "step returns to the level above, still on what was descended into"
    , example do
        screen <- driving (\_ session -> after session [MoveDown, Descend, Ascend])
        wide 6 screen === wide 6 start
        onKeys screen === ["Aphex Twin"]
    )
  ,
    ( "step keeps the level it is on when the library will not answer"
    , example do
        screen <- driving (\_ session -> stumbling session [Descend])
        take 5 (wide 7 screen) === take 5 (wide 7 start)
        onKeys screen === ["anohni"]
    )
  ,
    ( "step says in the strip why the library did not answer"
    , example do
        screen <- driving (\_ session -> stumbling session [Descend])
        showing screen.strip === Just (Wrong "The server could not be reached: down")
    )
  ,
    ( "step clears the strip on the next key press"
    , example do
        cleared <- driving $ \_ session -> do
          screen <- stumbling session [Descend]
          taking library session screen MoveDown
        showing cleared.strip === Nothing
    )
  ]

-- | What Enter on a song does to the audio under the screen.
picking :: Checks
picking =
  [
    ( "picking a song plays the song the selection is on"
    , example do
        (told, song) <- driving $ \standin session -> do
          _ <- after session playingDrukqs
          (,) <$> loaded standin <*> playing session
        told === [from btoumRoumada]
        song === Just btoumRoumada
    )
  ,
    ( "picking a song leaves the lists exactly where they were, but for the mark on it"
    , example do
        (browsing, picked) <- driving $ \_ session -> do
          browsing <- after session toDrukqs
          (,) browsing <$> taking library session browsing Descend
        unmarked (shown (60, 6) picked) === shown (60, 6) browsing
        onKeys picked === ["    ▶ Btoum Roumada"]
    )
  ,
    ( "picking a song then plays the rest of its album, with nothing more pressed"
    , example do
        told <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          _ <- ranOut standin session screen 2
          loaded standin
        told === fmap from drukqsSongs
    )
  ,
    ( "picking replaces what is playing when the song is of another album"
    , example do
        (told, song) <- driving $ \standin session -> do
          _ <- after session (playingDrukqs <> toSketches <> [Descend])
          (,) <$> loaded standin <*> playing session
        told === [from btoumRoumada, from untitled]
        song === Just untitled
    )
  ,
    ( "picking then follows that album to its end, and no further"
    , example do
        (told, song) <- driving $ \standin session -> do
          screen <- after session (playingDrukqs <> toSketches <> [Descend])
          _ <- ranOut standin session screen 3
          (,) <$> loaded standin <*> session.nowPlaying
        told === [from btoumRoumada, from untitled]
        fmap (.song) song === Nothing
    )
  ]

-- | Picking where the level has no song to play.
silent :: Checks
silent =
  [
    ( "picking plays nothing at a level that has no song to pick"
    , example do
        told <- driving $ \standin session -> do
          _ <- after session [MoveDown, Descend, Descend]
          loaded standin
        told === []
    )
  ,
    ( "picking plays nothing in an album the library holds no songs for"
    , example do
        told <- driving $ \standin session -> do
          _ <- after session [Descend, Descend, Descend]
          loaded standin
        told === []
    )
  ]

-- | Browsing on while a song of an album plays.
browsingOn :: Checks
browsingOn =
  [
    ( "browsing goes on moving, going back up and descending, and the song plays on"
    , example do
        (screen, told, song, motion) <- driving $ \standin session -> do
          screen <-
            resuming session playingDrukqs [MoveDown, MoveUp, Ascend, Ascend, MoveDown, Descend]
          (,,,) screen <$> loaded standin <*> playing session <*> motionOf standin
        trail screen === [["zebra"], []]
        told === [from btoumRoumada]
        song === Just btoumRoumada
        motion === Just Running
    )
  ,
    ( "browsing keeps playing that album while another artist's is browsed"
    , example do
        told <- driving $ \standin session -> do
          screen <- resuming session playingDrukqs [Ascend, Ascend, MoveUp, Descend, Descend]
          _ <- ranOut standin session screen 2
          loaded standin
        told === fmap from drukqsSongs
    )
  ]

-- | What leaving the player or the account does to the audio.
leaving :: Checks
leaving =
  [
    ( "leaving the player stops the audio on the way out"
    , example do
        (ending, song, motion) <- driving $ \standin session -> do
          ending <- ends session Leave playingDrukqs
          (,,) ending <$> session.nowPlaying <*> motionOf standin
        ending === Just Quit
        (fmap (.song) song, motion) === (Nothing, Nothing)
    )
  ,
    ( "leaving the account stops it on the way to the login screen just the same"
    , example do
        (ending, song, motion) <- driving $ \standin session -> do
          ending <- ends session LogOut playingDrukqs
          (,,) ending <$> session.nowPlaying <*> motionOf standin
        ending === Just LoggedOut
        (fmap (.song) song, motion) === (Nothing, Nothing)
    )
  ,
    ( "leaving the account logs out of one with nothing playing"
    , example do
        (ending, told, song, motion) <- driving $ \standin session -> do
          ending <- ends session LogOut toDrukqs
          (,,,) ending <$> loaded standin <*> session.nowPlaying <*> motionOf standin
        ending === Just LoggedOut
        told === []
        (fmap (.song) song, motion) === (Nothing, Nothing)
    )
  ]

-- | Space, which holds the audio and lets it run on from where it stopped.
pausing :: Checks
pausing =
  [
    ( "space holds the audio, and freezes the elapsed time with it"
    , example do
        (motion, at) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 40)
          _ <- pressing session screen [PauseOrResume]
          (,) <$> motionOf standin <*> elapsed session
        motion === Just Paused
        at === Just (Seconds 40)
    )
  ,
    ( "space again lets it run on from the same point"
    , example do
        (motion, at) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 40)
          _ <- pressing session screen [PauseOrResume, PauseOrResume]
          (,) <$> motionOf standin <*> elapsed session
        motion === Just Running
        at === Just (Seconds 40)
    )
  ]

-- | n and p, which move the playing along the album.
skipping :: Checks
skipping =
  [
    ( "n plays the next song of the album"
    , example do
        (song, told) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          _ <- pressing session screen [NextSong]
          (,) <$> playing session <*> loaded standin
        song === Just jynweythek
        told === fmap from (take 2 drukqsSongs)
    )
  ,
    ( "n on the album's last song ends the playing"
    , example do
        (song, motion) <- driving $ \standin session -> do
          screen <- after session (toDrukqs <> [MoveDown, MoveDown, Descend])
          _ <- pressing session screen [NextSong]
          (,) <$> session.nowPlaying <*> motionOf standin
        (fmap (.song) song, motion) === (Nothing, Nothing)
    )
  ,
    ( "p goes back a song, part-way through the one playing"
    , example do
        (song, told) <- driving $ \standin session -> do
          screen <- after session (toDrukqs <> [MoveDown, Descend])
          reach standin (Seconds 60)
          _ <- pressing session screen [PreviousSong]
          (,) <$> playing session <*> loaded standin
        song === Just btoumRoumada
        told === [from jynweythek, from btoumRoumada]
    )
  ,
    ( "p on the first song plays the first song again"
    , example do
        (song, told) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 60)
          _ <- pressing session screen [PreviousSong]
          (,) <$> playing session <*> loaded standin
        song === Just btoumRoumada
        told === [from btoumRoumada, from btoumRoumada]
    )
  ]

-- | The arrows, which move the playing song along itself.
seeks :: Checks
seeks =
  [
    ( "the arrows move the song 5s, and the elapsed time with it"
    , example do
        (forward, back) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 40)
          _ <- pressing session screen [Seek 5]
          forward <- elapsed session
          _ <- pressing session screen [Seek (-5)]
          (,) forward <$> elapsed session
        forward === Just (Seconds 45)
        back === Just (Seconds 40)
    )
  ,
    ( "shift held moves it 30s"
    , example do
        (forward, back) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 40)
          _ <- pressing session screen [Seek 30]
          forward <- elapsed session
          _ <- pressing session screen [Seek (-30)]
          (,) forward <$> elapsed session
        forward === Just (Seconds 70)
        back === Just (Seconds 40)
    )
  ,
    ( "a seek stops at the end of the track, which then finishes into the next song"
    , example do
        (at, told, song) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 90)
          stepped <- pressing session screen [Seek 30]
          at <- elapsed session
          told <- loaded standin
          _ <- ranOut standin session stepped 1
          (,,) at told <$> playing session
        at === Just (Seconds 96)
        told === [from btoumRoumada]
        song === Just jynweythek
    )
  ,
    ( "a seek stops at the start of the track, and reaches no earlier song"
    , example do
        (at, song, told) <- driving $ \standin session -> do
          screen <- after session playingDrukqs
          reach standin (Seconds 10)
          _ <- pressing session screen [Seek (-30)]
          (,,) <$> elapsed session <*> playing session <*> loaded standin
        at === Just (Seconds 0)
        song === Just btoumRoumada
        told === [from btoumRoumada]
    )
  ]

-- | What the controls leave exactly as they found it.
leftAlone :: Checks
leftAlone =
  [
    ( "the controls leave the selection and the level as they were, at every level"
    , example do
        unmoved <- driving $ \_ session ->
          forM [[], [Ascend], [Ascend, Ascend]] $ \path -> do
            browsing <- after session (playingDrukqs <> path)
            controlled <- pressing session browsing controls
            pure (shown (60, 6) controlled, shown (60, 6) browsing)
        assert (all (uncurry (==)) unmoved)
    )
  ,
    ( "the controls do nothing at all with nothing playing"
    , example do
        (told, song, motion, controlled, browsing) <- driving $ \standin session -> do
          browsing <- after session toDrukqs
          controlled <- pressing session browsing controls
          told <- loaded standin
          song <- session.nowPlaying
          motion <- motionOf standin
          pure (told, song, motion, controlled, browsing)
        told === []
        (fmap (.song) song, motion) === (Nothing, Nothing)
        shown (60, 6) controlled === shown (60, 6) browsing
    )
  ]

-- | What the overlay shows of the song playing.
overlay :: Checks
overlay =
  [
    ( "the overlay shows the playing song's name and its elapsed and total time"
    , example do
        screen <- driving onDrukqs
        stripRow screen === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
    )
  ,
    ( "the overlay moves the elapsed time on as the audio does"
    , example do
        moved <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 42)
          beaten session 1 screen
        stripRow moved === "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"
    )
  ,
    ( "the overlay leaves the elapsed time where a held song left it"
    , example do
        held <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 42)
          running <- beaten session 1 screen
          session.pause
          beaten session 2 running >>= beaten session 3
        stripRow held === "⏸ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"
    )
  ,
    ( "the overlay shows the next song of the album once playback moves on"
    , example do
        moved <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          ranOut standin session screen 1
        stripRow moved === "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
    )
  ,
    ( "the overlay is not on screen while nothing is playing"
    , example do
        screen <- driving (\_ session -> after session toDrukqs >>= beaten session 0)
        onStrip screen === Nothing
    )
  ,
    ( "the overlay is there at every one of the three levels"
    , example do
        rows <- driving $ \standin session -> do
          songs <- onDrukqs standin session
          albums <- taking library session songs Ascend >>= beaten session 1
          names <- taking library session albums Ascend >>= beaten session 2
          pure (fmap stripRow [songs, albums, names])
        rows === replicate 3 ("⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")
    )
  ]

-- | Where the overlay sits, and when it is not there at all.
placing :: Checks
placing =
  [
    ( "the overlay takes the bottom row inside the margin and the row above it, no more"
    , example do
        (started, browsing) <- driving $ \standin session -> do
          browsing <- after session toDrukqs
          picked <- taking library session browsing Descend
          begin standin
          (,) <$> beaten session 0 picked <*> pure browsing
        unmarked (shown (60, 6) started)
          === shown (60, 4) browsing <> ["", "⏵ Btoum Roumada  " <> bar 0 30 <> "  0:00 / 1:36"]
    )
  ,
    ( "the overlay is gone when the album's last song finishes, the list left where it was"
    , example do
        (ended, browsing, song) <- driving $ \standin session -> do
          browsing <- after session toDrukqs
          started <- taking library session browsing Descend >>= beaten session 0
          ended <- ranOut standin session started 3
          (,,) ended browsing <$> session.nowPlaying
        onStrip ended === Nothing
        fmap (.song) song === Nothing
        shown (60, 6) ended === shown (60, 6) browsing
    )
  ]

-- | The loading indicator a song picked with Enter is given.
indicator :: Checks
indicator =
  [
    ( "a song picked with Enter shows a loading indicator in place of the elapsed time"
    , example do
        screen <- driving (\_ session -> loadingDrukqs session)
        stripRow screen === "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"
    )
  ,
    ( "the elapsed time takes its place once the audio starts, moving on as it plays"
    , example do
        (started, moved) <- driving $ \standin session -> do
          screen <- loadingDrukqs session
          begin standin
          started <- beaten session 1 screen
          reach standin (Seconds 42)
          (,) started <$> beaten session 2 started
        stripRow started === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
        stripRow moved === "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"
    )
  ,
    ( "the indicator shows for a song picked while another still loads, until that one starts"
    , example do
        (repicked, started) <- driving $ \standin session -> do
          screen <- loadingDrukqs session
          repicked <- pressing session screen [MoveDown, Descend] >>= beaten session 1
          begin standin
          (,) repicked <$> beaten session 2 repicked
        stripRow repicked === "  Jynweythek  " <> bar 0 19 <> "     ⠋ / 2:09"
        stripRow started === "⏵ Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
    )
  ]

-- | The songs that are given no indicator, and space pressed over one.
noIndicator :: Checks
noIndicator =
  [
    ( "the indicator is never shown for the song the album moves on to by itself"
    , example do
        moved <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          ranOut standin session screen 1
        stripRow moved === "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
    )
  ,
    ( "nor for the songs n and p move to, even away from a song still loading"
    , example do
        (forward, back) <- driving $ \_ session -> do
          screen <- loadingDrukqs session
          forward <- pressing session screen [NextSong] >>= beaten session 1
          (,) forward <$> (pressing session forward [PreviousSong] >>= beaten session 2)
        stripRow forward === "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
        stripRow back === "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
    )
  ,
    ( "space while it loads finishes it held at 0:00, until space again plays it from its start"
    , example do
        (holding, ready, wasHeld, nowRunning, at, told) <- driving $ \standin session -> do
          screen <- loadingDrukqs session
          holding <- pressing session screen [PauseOrResume] >>= beaten session 1
          begin standin
          ready <- beaten session 2 holding >>= beaten session 60
          wasHeld <- motionOf standin
          _ <- pressing session ready [PauseOrResume]
          (,,,,,) holding ready wasHeld <$> motionOf standin <*> elapsed session <*> loaded standin
        stripRow holding === "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"
        stripRow ready === "⏸ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
        wasHeld === Just Paused
        nowRunning === Just Running
        at === Just (Seconds 0)
        told === [from btoumRoumada]
    )
  ]

-- | A playback failure, which is given the whole strip.
complaints :: Checks
complaints =
  [
    ( "a file that will not play is given the whole strip"
    , example do
        failed <- driving $ \standin session -> do
          screen <- loadingDrukqs session
          breakWith standin (Unplayable "the file will not play: it is corrupt")
          beaten session 1 screen
        onStrip failed === Just (Wrong "The file will not play: it is corrupt")
    )
  ,
    ( "a server that cannot be reached is given it just the same"
    , example do
        failed <- driving $ \standin session -> do
          screen <- loadingDrukqs session
          breakWith standin (Unreachable "the server could not be reached: it is down")
          beaten session 1 screen
        onStrip failed === Just (Wrong "The server could not be reached: it is down")
    )
  ]

-- | The part of the bar the audio has filled.
progress :: Checks
progress =
  [
    ( "the progress bar sits on the strip's one line, between the track name and the times"
    , example do
        screen <- driving onDrukqs
        stripRow screen === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
    )
  ,
    ( "the progress bar fills as the track plays, by the part of it that has played"
    , example do
        (partWay, filled) <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          let at seconds = reach standin (Seconds seconds) >> beaten session 1 screen
          partWay <- at 48
          (,) partWay <$> traverse (fmap (filledIn . stripRow) . at) [6, 24, 72, 90]
        stripRow partWay === "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
        filled === [1, 4, 12, 15]
    )
  ,
    ( "the progress bar stands still with the elapsed time while held, then moves on again"
    , example do
        (held, wasHeld, moved, running) <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 48)
          held <- pressing session screen [PauseOrResume] >>= beaten session 1 >>= beaten session 2
          wasHeld <- motionOf standin
          resumed <- pressing session held [PauseOrResume]
          running <- motionOf standin
          reach standin (Seconds 72)
          (,,,) held wasHeld <$> beaten session 3 resumed <*> pure running
        wasHeld === Just Paused
        stripRow held === "⏸ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
        running === Just Running
        stripRow moved === "⏵ Btoum Roumada  " <> bar 12 4 <> "  1:12 / 1:36"
    )
  ,
    ( "the progress bar moves forward and back with a seek, as far as the elapsed time"
    , example do
        rows <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 48)
          let seeking sought by = pressing session sought [Seek by] >>= beaten session 1
          forward <- seeking screen 30
          back <- seeking forward (-30)
          nudgedBack <- seeking back (-5)
          nudged <- seeking nudgedBack 5
          pure (fmap stripRow [forward, back, nudgedBack, nudged])
        rows
          === [ "⏵ Btoum Roumada  " <> bar 13 3 <> "  1:18 / 1:36"
              , "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
              , "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:43 / 1:36"
              , "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
              ]
    )
  ]

-- | The bar at either end of the track, at other widths, and given way.
limits :: Checks
limits =
  [
    ( "the progress bar stays at whichever end of the track a seek past it stops at"
    , example do
        (ended, started) <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 90)
          ended <- pressing session screen [Seek 30] >>= beaten session 1
          reach standin (Seconds 10)
          (,) ended <$> (pressing session ended [Seek (-30)] >>= beaten session 2)
        stripRow ended === "⏵ Btoum Roumada  " <> bar 16 0 <> "  1:36 / 1:36"
        stripRow started === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
    )
  ,
    ( "the progress bar gives way, with the track name and the times, to a playback error"
    , example do
        screen <-
          driving (breaking (Unplayable "the file will not play: it is corrupt"))
        stripRow screen === "The file will not play: it is corrupt"
    )
  ,
    ( "the progress bar stays empty for a track the server gives no length for"
    , example do
        rows <- driving $ \standin session -> do
          picked <- after session toSilence
          begin standin
          screen <- beaten session 0 picked
          later <- beaten session 10 screen
          pure (fmap stripRow [screen, later])
        rows === replicate 2 ("⏵ Silence  " <> bar 0 22 <> "  0:00 / 0:00")
    )
  ,
    ( "the progress bar shrinks and grows with the width of the screen"
    , example do
        partWay <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          reach standin (Seconds 48)
          beaten session 1 screen
        let stripAt width = mconcat (drop 4 (shown (width, 5) partWay))
        stripAt 32 === "⏵ Btoum Roumada  " <> bar 1 1 <> "  0:48 / 1:36"
        stripAt 82 === "⏵ Btoum Roumada  " <> bar 26 26 <> "  0:48 / 1:36"
    )
  ,
    ( "the strip never wraps onto a second row, however narrow the screen"
    , example do
        (started, browsing) <- driving $ \standin session -> do
          browsing <- after session toDrukqs
          picked <- taking library session browsing Descend
          begin standin
          (,) <$> beaten session 0 picked <*> pure browsing
        unmarked (shown (20, 6) started)
          === shown (20, 4) browsing <> ["", "⏵ Btoum Roumada    0"]
    )
  ]

-- | What a skipped track or an unreachable server leaves in the strip.
reasons :: Checks
reasons =
  [
    ( "a skipped track's reason goes in place of the overlay's contents"
    , example do
        screen <- driving (breaking (Unplayable "the file will not play: it is corrupt"))
        onStrip screen === Just (Wrong "The file will not play: it is corrupt")
    )
  ,
    ( "the strip goes back to the next song's line a few seconds on"
    , example do
        (later, told) <- driving $ \standin session -> do
          screen <- breaking (Unplayable "the file will not play: it is corrupt") standin session
          later <- beaten session 10 screen
          (,) later <$> loaded standin
        stripRow later === "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
        told === fmap from (take 2 drukqsSongs)
    )
  ,
    ( "a network failure's reason stays there however long it is left"
    , example do
        (waited, song) <- driving $ \standin session -> do
          screen <- breaking (Unreachable "the server could not be reached: it is down") standin session
          waited <- beaten session 600 screen
          (,) waited <$> session.nowPlaying
        onStrip waited === Just (Wrong "The server could not be reached: it is down")
        fmap (.song) song === Nothing
    )
  ,
    ( "the next key press takes it down, and still does its usual job"
    , example do
        moved <- driving $ \standin session -> do
          screen <- breaking (Unreachable "the server could not be reached: it is down") standin session
          taking library session screen MoveDown
        onStrip moved === Nothing
        onKeys moved === ["  1   Jynweythek"]
    )
  ]

-- | The playing and paused symbols while the audio runs, is held, or loads.
eitherSymbol :: Checks
eitherSymbol =
  [
    ( "one symbol shows while the audio runs and another while it is held, switched by each space"
    , example do
        symbols <- driving $ \standin session -> do
          running <- onDrukqs standin session
          held <- pressing session running [PauseOrResume] >>= beaten session 1
          again <- pressing session held [PauseOrResume] >>= beaten session 2
          pure (fmap standing [running, held, again])
        symbols === [["⏵"], ["⏸"], ["⏵"]]
    )
  ,
    ( "neither shows while a song picked with Enter loads, space pressed or not"
    , example do
        (rows, symbols) <- driving $ \_ session -> do
          loading <- loadingDrukqs session
          holding <- pressing session loading [PauseOrResume] >>= beaten session 1
          pure (fmap stripRow [loading, holding], fmap standing [loading, holding])
        rows === replicate 2 ("  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36")
        symbols === [[], []]
    )
  ,
    ( "neither shows while a song reached by n or p, or moved on to by the album, loads"
    , example do
        (rows, symbols) <- driving $ \standin session -> do
          screen <- onDrukqs standin session
          forward <- pressing session screen [NextSong] >>= beaten session 1
          back <- pressing session forward [PreviousSong] >>= beaten session 2
          movedOn <- ranOut standin session back 1
          pure (fmap stripRow [forward, back, movedOn], fmap standing [forward, back, movedOn])
        rows
          === [ "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
              , "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
              , "  Jynweythek  " <> bar 0 19 <> "  0:00 / 2:09"
              ]
        symbols === [[], [], []]
    )
  ]

-- | Which symbol a song that has finished loading is left with.
onceLoaded :: Checks
onceLoaded =
  [
    ( "the playing one shows once a song loads with no space pressed, however it started"
    , example do
        symbols <- driving $ \standin session -> do
          picked <- loadingDrukqs session
          begin standin
          pickedBegun <- beaten session 1 picked
          forward <- pressing session pickedBegun [NextSong]
          begin standin
          forwardBegun <- beaten session 2 forward
          movedOn <- ranOut standin session forwardBegun 1
          begin standin
          movedOnBegun <- beaten session 3 movedOn
          pure (fmap standing [pickedBegun, forwardBegun, movedOnBegun])
        symbols === replicate 3 ["⏵"]
    )
  ,
    ( "the paused one shows once a song loads with space pressed while it loaded, however started"
    , example do
        (symbols, motion) <- driving $ \standin session -> do
          picked <- loadingDrukqs session >>= flip (pressing session) [PauseOrResume]
          begin standin
          pickedBegun <- beaten session 1 picked
          forward <- pressing session pickedBegun [NextSong, PauseOrResume]
          begin standin
          forwardBegun <- beaten session 2 forward
          movedOn <- ranOut standin session forwardBegun 1 >>= flip (pressing session) [PauseOrResume]
          begin standin
          movedOnBegun <- beaten session 3 movedOn
          (,) (fmap standing [pickedBegun, forwardBegun, movedOnBegun]) <$> motionOf standin
        symbols === replicate 3 ["⏸"]
        motion === Just Paused
    )
  ]

-- | When neither symbol is on screen.
noSymbol :: Checks
noSymbol =
  [
    ( "neither shows while a skipped track's reason is in the strip, the next song behind it"
    , example do
        behind <- driving $ \standin session -> do
          failed <- breaking (Unplayable "the file will not play: it is corrupt") standin session
          begin standin
          beaten session 1 failed
        onStrip behind === Just (Wrong "The file will not play: it is corrupt")
        standing behind === []
    )
  ,
    ( "neither shows while a network failure's reason is in the strip"
    , example do
        failed <- driving (breaking (Unreachable "the server could not be reached: it is down"))
        onStrip failed === Just (Wrong "The server could not be reached: it is down")
        standing failed === []
    )
  ,
    ( "neither is on screen with nothing playing, and neither is the strip"
    , example do
        opened <- driving (\_ session -> after session toDrukqs >>= beaten session 0)
        (onStrip opened, standing opened) === (Nothing, [])
        finished <- driving $ \standin session ->
          onDrukqs standin session >>= \started -> ranOut standin session started 3
        (onStrip finished, standing finished) === (Nothing, [])
        ended <- driving $ \standin session ->
          onDrukqs standin session
            >>= flip (pressing session) [NextSong, NextSong, NextSong]
            >>= beaten session 1
        (onStrip ended, standing ended) === (Nothing, [])
    )
  ]

-- | Where the mark on the playing song is.
marking :: Checks
marking =
  [
    ( "the mark is one space before its name, with the selection on it"
    , example do
        screen <- driving onDrukqs
        wide 7 screen
          === [ across ["Artists", "Albums", "Songs"]
              , rules 3
              , across ["anohni", "      Sketches", "    ▶ Btoum Roumada"]
              , across ["Aphex Twin", "1992  Selected Ambient Works 85-92", "  1   Jynweythek"]
              , across ["zebra", "2001  Drukqs", "  2   Vordhosbn"]
              , ""
              , "⏵ Btoum Roumada  " <> bar 0 90 <> "  0:00 / 1:36"
              ]
        onKeys screen === ["    ▶ Btoum Roumada"]
    )
  ,
    ( "the mark stays on it with the selection moved off it, and on no other row"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= flip (pressing session) [MoveDown, MoveDown]
        carrying screen === ["    ▶ Btoum Roumada"]
        onKeys screen === ["  2   Vordhosbn"]
    )
  ,
    ( "the mark is on a picked song from Enter, while it is still loading"
    , example do
        screen <- driving (\_ session -> after session playingDrukqs)
        carrying screen === ["    ▶ Btoum Roumada"]
    )
  ,
    ( "the mark stays where it is when the song is paused"
    , example do
        (screen, motion) <- driving $ \standin session -> do
          screen <-
            onDrukqs standin session >>= flip (pressing session) [PauseOrResume] >>= beaten session 1
          (,) screen <$> motionOf standin
        motion === Just Paused
        carrying screen === ["    ▶ Btoum Roumada"]
    )
  ,
    ( "the mark is still there after Esc out of its album and Enter back into it"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= flip (pressing session) [Ascend, Descend]
        songsOf screen === Just ("Aphex Twin", "2001  Drukqs")
        carrying screen === ["    ▶ Btoum Roumada"]
    )
  ]

-- | The songs and albums the mark is never on.
elsewhere :: Checks
elsewhere =
  [
    ( "the mark is on no song of another album of the same artist"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= flip (pressing session) toSketches
        songsOf screen === Just ("Aphex Twin", "      Sketches")
        carrying screen === []
    )
  ,
    ( "the mark is on no song of another artist's album"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session
            >>= flip (pressing session) [Ascend, Ascend, MoveUp, Descend, MoveDown, Descend]
        songsOf screen === Just ("anohni", "2017  Paradise")
        carrying screen === []
    )
  ]

-- | Where the mark moves as the playing does.
moving :: Checks
moving =
  [
    ( "the mark moves on with the album when a song finishes by itself"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= \started -> ranOut standin session started 1
        carrying screen === ["  1 ▶ Jynweythek"]
    )
  ,
    ( "the mark moves to the next song on n"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= flip (pressing session) [NextSong]
        carrying screen === ["  1 ▶ Jynweythek"]
    )
  ,
    ( "the mark moves to the previous song on p"
    , example do
        screen <- driving (\_ session -> after session (toDrukqs <> [MoveDown, Descend, PreviousSong]))
        carrying screen === ["    ▶ Btoum Roumada"]
    )
  ,
    ( "the mark moves past a skipped track onto the song that plays instead"
    , example do
        screen <- driving (breaking (Unplayable "the file will not play: it is corrupt"))
        carrying screen === ["  1 ▶ Jynweythek"]
    )
  ,
    ( "the mark moves to another song picked with Enter"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= flip (pressing session) [MoveDown, MoveDown, Descend]
        carrying screen === ["  2 ▶ Vordhosbn"]
    )
  ]

-- | When no song is marked at all.
gone :: Checks
gone =
  [
    ( "the mark is on no song once the album's last song has finished"
    , example do
        screen <- driving $ \standin session ->
          onDrukqs standin session >>= \started -> ranOut standin session started 3
        songsOf screen === Just ("Aphex Twin", "2001  Drukqs")
        carrying screen === []
    )
  ,
    ( "the mark is on no song once n on the last song has ended the playing"
    , example do
        screen <-
          driving (\_ session -> after session (toDrukqs <> [MoveDown, MoveDown, Descend, NextSong]))
        carrying screen === []
    )
  ,
    ( "the mark is on no song once a network failure has stopped the playing"
    , example do
        screen <- driving (breaking (Unreachable "the server could not be reached: it is down"))
        carrying screen === []
    )
  ,
    ( "the mark is never on an album or an artist"
    , example do
        marked <- driving $ \standin session -> do
          albums <- onDrukqs standin session >>= flip (pressing session) [Ascend]
          names <- pressing session albums [Ascend]
          pure (fmap carrying [albums, names])
        marked === [[], []]
    )
  ,
    ( "the mark is on no song after logging out and in again"
    , example do
        (ending, loggedIn) <- driving $ \_ session -> do
          ending <- ends session LogOut playingDrukqs
          (,) ending <$> after session toDrukqs
        ending === Just LoggedOut
        carrying loggedIn === []
    )
  ]

-- | How the levels sit side by side on the terminal.
columns :: Checks
columns =
  [
    ( "the columns open on the artist list alone, one column at the left"
    , example do
        wide 6 start === ["Artists", rules 1, "anohni", "Aphex Twin", "zebra", ""]
        assert (all ((<= 40) . T.length) (wide 6 start))
    )
  ,
    ( "the columns put the keys on the first artist, and on no other row"
    , example do
        screen <- driving (\_ session -> after session [MoveDown])
        onKeys start === ["anohni"]
        onKeys screen === ["Aphex Twin"]
    )
  ,
    ( "the columns put an artist's albums in a second, the artist highlighted in the first"
    , example do
        screen <- driving (\_ session -> after session [MoveDown, Descend])
        wide 6 screen
          === [ across ["Artists", "Albums"]
              , rules 2
              , across ["anohni", "      Sketches"]
              , across ["Aphex Twin", "1992  Selected Ambient Works 85-92"]
              , across ["zebra", "2001  Drukqs"]
              , across ["", ""]
              ]
        trail screen === [["Aphex Twin"], ["      Sketches"]]
    )
  ,
    ( "the columns put an album's songs in a third, the artist and the album highlighted"
    , example do
        screen <- driving (\_ session -> after session toDrukqs)
        wide 6 screen === drukqsColumns
        trail screen === [["Aphex Twin"], ["2001  Drukqs"], ["      Btoum Roumada"]]
    )
  ]

-- | Which row of which column is highlighted.
rowPicked :: Checks
rowPicked =
  [
    ( "the keys move in the rightmost column, and no highlighted row left of it"
    , example do
        (movedSongs, movedAlbums) <- driving $ \_ session -> do
          songs <- after session toDrukqs
          movedSongs <- pressing session songs [MoveDown, MoveDown, MoveUp]
          albums <- after session [MoveDown, Descend]
          (,) movedSongs <$> pressing session albums [MoveDown, MoveDown]
        trail movedSongs === [["Aphex Twin"], ["2001  Drukqs"], ["  1   Jynweythek"]]
        trail movedAlbums === [["Aphex Twin"], ["2001  Drukqs"]]
    )
  ,
    ( "the rows picked left of the keys are drawn just as the keys' row, and none of them bold"
    , example do
        (albums, songs) <- driving $ \_ session ->
          (,) <$> after session [MoveDown, Descend] <*> after session toDrukqs
        fmap reversed (drawnAs "      Sketches" albums) === [True]
        drawnAs "Aphex Twin" albums === drawnAs "      Sketches" albums
        emboldened albums === ["Artists", "Albums"]
        fmap reversed (drawnAs "      Btoum Roumada" songs) === [True]
        drawnAs "Aphex Twin" songs === drawnAs "      Btoum Roumada" songs
        drawnAs "2001  Drukqs" songs === drawnAs "      Btoum Roumada" songs
        emboldened songs === ["Artists", "Albums", "Songs"]
    )
  ]

-- | What Esc leaves of the columns.
returning :: Checks
returning =
  [
    ( "Esc loses the rightmost, leaving the rest exactly as they were left"
    , example do
        (albums, artistsAlone, unmoved, leftAtAlbums, leftAtArtists) <- driving $ \_ session -> do
          songs <- after session toDrukqs
          albums <- pressing session songs [Ascend]
          artistsAlone <- pressing session albums [Ascend]
          unmoved <- pressing session artistsAlone [Ascend]
          leftAtAlbums <- after session [MoveDown, Descend, MoveDown, MoveDown]
          (,,,,) albums artistsAlone unmoved leftAtAlbums <$> after session [MoveDown]
        looks albums === looks leftAtAlbums
        looks artistsAlone === looks leftAtArtists
        looks unmoved === looks artistsAlone
    )
  ,
    ( "Esc gives another album's songs, the albums changed only in their mark"
    , example do
        screen <- driving (\_ session -> after session (toDrukqs <> [Ascend, MoveUp, Descend]))
        wide 6 screen === ambientColumns
        trail screen === [["Aphex Twin"], ["1992  Selected Ambient Works 85-92"], ["  1   Xtal"]]
    )
  ]

-- | Each column scrolling on its own.
scrolling :: Checks
scrolling =
  [
    ( "the columns scroll each on its own"
    , example do
        (artistsScrolled, albums, albumsScrolled) <- driving $ \_ session -> do
          let crowded =
                Library
                  { Library.artists = pure (Right [])
                  , Library.albums =
                      const . pure . Right $
                        [ album "First" "First" (Just 2001)
                        , album "Second" "Second" (Just 2002)
                        , album "Third" "Third" (Just 2003)
                        , album "Fourth" "Fourth" (Just 2004)
                        , album "Fifth" "Fifth" (Just 2005)
                        ]
                  , Library.songs = const (pure (Right []))
                  }
              crowding = foldM (taking crowded session)
              many =
                opening (fmap (\name -> artist name name) ["one", "two", "three", "four", "five", "six"])
          artistsScrolled <- crowding many [MoveDown, MoveDown, MoveDown, MoveDown]
          albums <- crowding artistsScrolled [Descend]
          (,,) artistsScrolled albums <$> crowding albums [MoveDown, MoveDown, MoveDown]
        wide 5 artistsScrolled === ["Artists", rules 1, "three", "four", "five"]
        wide 5 albums
          === [ across ["Artists", "Albums"]
              , rules 2
              , across ["three", "2001  First"]
              , across ["four", "2002  Second"]
              , across ["five", "2003  Third"]
              ]
        wide 5 albumsScrolled
          === [ across ["Artists", "Albums"]
              , rules 2
              , across ["three", "2002  Second"]
              , across ["four", "2003  Third"]
              , across ["five", "2004  Fourth"]
              ]
    )
  ]

-- | A column with nothing in it, and the columns alongside a playing song.
alongside :: Checks
alongside =
  [
    ( "an artist with no albums gives an empty second column, left again on Esc"
    , example do
        (screen, back, leftAtArtists) <- driving $ \_ session -> do
          screen <- after session [MoveDown, MoveDown, Descend]
          back <- pressing session screen [Ascend]
          (,,) screen back <$> after session [MoveDown, MoveDown]
        wide 6 screen
          === [ across ["Artists", "Albums"]
              , rules 2
              , across ["anohni", ""]
              , across ["Aphex Twin", ""]
              , across ["zebra", ""]
              , across ["", ""]
              ]
        onKeys screen === []
        looks back === looks leftAtArtists
        onKeys back === ["zebra"]
    )
  ,
    ( "the columns all behave the same with a song playing, which plays on"
    , example do
        (caught, artistsAlone, song, motion) <- driving $ \standin session -> do
          screen <- resuming session playingDrukqs [MoveDown, Ascend, MoveUp, Descend]
          begin standin
          caught <- beaten session 1 screen
          artistsAlone <- pressing session caught [Ascend, Ascend]
          (,,,) caught artistsAlone <$> playing session <*> motionOf standin
        let showing' = "⏵ Btoum Roumada  " <> bar 0 90 <> "  0:00 / 1:36"
        wide 8 caught === ambientColumns <> ["", showing']
        emboldened caught === ["Artists", "Albums", "Songs", showing']
        trail caught === [["Aphex Twin"], ["1992  Selected Ambient Works 85-92"], ["  1   Xtal"]]
        take 5 (wide 7 artistsAlone) === take 5 (wide 7 start)
        onKeys artistsAlone === ["Aphex Twin"]
        song === Just btoumRoumada
        motion === Just Running
    )
  ]

-- | A row too long for its column, and the blank row above a strip.
shortened :: Checks
shortened =
  [
    ( "a row too long for its column is shortened, and none wraps onto another line"
    , example do
        screen <- driving (\_ session -> after session toDrukqs)
        shown (24, 6) screen
          === [ "Artists │Albums │Songs"
              , "────────│───────│───────"
              , "anohni  │      …│      …"
              , "Aphex T…│1992  …│  1   …"
              , "zebra   │2001  …│  2   …"
              , "        │       │"
              ]
        let broken = opening [artist "x" "one\ntwo", artist "y" "three"]
        wide 4 broken === ["Artists", rules 1, "one two", "three"]
    )
  ,
    ( "a strip on screen has a blank row between it and the columns, at every size"
    , property do
        region <- forAll roomy
        situation <- forAll (Gen.element [Loading, Sounding, Broken])
        screen <- driving (screenIn situation)
        let rows = within region screen
        assert (all (all vacant) (take 1 (drop (snd region - 2) rows)))
    )
  ]

-- | The lines under the headings, the rule between columns, and the strip.
headings :: Checks
headings =
  [
    ( "the Artists heading has a line under it, the first artist on the row below"
    , example (take 3 (wide 6 start) === ["Artists", rules 1, "anohni"])
    )
  ,
    ( "each of the three headings has a line under it, all on the one row"
    , example do
        screen <- driving (\_ session -> after session toDrukqs)
        take 3 (wide 6 screen)
          === [ across ["Artists", "Albums", "Songs"]
              , rules 3
              , across ["anohni", "      Sketches", "      Btoum Roumada"]
              ]
    )
  ,
    ( "the rule between two columns is drawn on the line's row as on every other row"
    , example do
        screen <- driving (\_ session -> after session toDrukqs)
        let rule at = do
              let drawn = downColumn 6 at screen
              fmap snd drawn === replicate 6 '│'
              length (group drawn) === 1
        rule 40
        rule 80
    )
  ,
    ( "what went wrong is kept in the strip along the bottom"
    , example do
        screen <- driving (\_ session -> stumbling session [Descend])
        drop 5 (wide 6 screen) === ["The server could not be reached: down"]
    )
  ]

-- | The blank cell around every screen.
margin :: Checks
margin =
  [
    ( "the terminal's outer rows and columns are blank at every level, the columns inside"
    , example do
        (albums, songs) <- driving $ \_ session ->
          (,) <$> after session [MoveDown, Descend] <*> after session toDrukqs
        assert (all (all vacant . border . whole (122, 8)) [start, albums, songs])
        screenshot (whole (122, 8) songs) === [""] <> fmap (" " <>) drukqsColumns <> [""]
    )
  ,
    ( "a song's strip is on the row above the blank bottom row, with a blank row above it"
    , example do
        screen <- driving onDrukqs
        let rows = whole (46, 8) screen
        assert (all vacant (border rows))
        drop 5 (screenshot rows) === ["", " ⏵ Btoum Roumada  " <> bar 0 14 <> "  0:00 / 1:36", ""]
        assert (all (all vacant) (take 1 (drop 5 rows)))
    )
  ,
    ( "an error in the strip is in just the same place, with the same blank rows"
    , example do
        (failed, unanswered) <- driving $ \standin session ->
          (,)
            <$> breaking (Unplayable "the file will not play: it is corrupt") standin session
            <*> stumbling session [Descend]
        let placed (screen, said) = do
              let rows = whole (46, 8) screen
              assert (all vacant (border rows))
              drop 5 (screenshot rows) === ["", " " <> said, ""]
              assert (all (all vacant) (take 1 (drop 5 rows)))
        placed (failed, "The file will not play: it is corrupt")
        placed (unanswered, "The server could not be reached: down")
    )
  ,
    ( "the margin stays blank whatever the terminal's width and height"
    , property do
        region <- forAll size
        situation <- forAll Gen.enumBounded
        screen <- driving (screenIn situation)
        filter (not . vacant) (border (whole region screen)) === []
    )
  ]

-- | A run against a stand-in backend, over a session of its own.
driving :: (Standin -> Session -> IO a) -> PropertyT IO a
driving use = evalIO (withStandin use)

-- | The screen a run opens on, over the stand-in library.
start :: Screen
start = opening artists

-- | One of the situations a screen can be in, so that what holds of every
-- screen can be asked of each of them.
data Situation
  = -- | The artist list a run opens on, with nothing playing.
    Opened
  | -- | An artist's albums, with nothing playing.
    Browsing
  | -- | A song picked with Enter, still loading.
    Loading
  | -- | A song playing, its overlay in the strip.
    Sounding
  | -- | A song that would not play, its reason in the strip.
    Broken
  deriving stock (Eq, Show, Enum, Bounded)

-- | The screen that situation leaves behind.
screenIn :: Situation -> Standin -> Session -> IO Screen
screenIn situation standin session = case situation of
  Opened -> pure start
  Browsing -> after session [MoveDown, Descend]
  Loading -> loadingDrukqs session
  Sounding -> onDrukqs standin session
  Broken -> breaking (Unplayable "the file will not play: it is corrupt") standin session

-- | Terminals from none at all, through ones too small to hold anything
-- inside their margin, to ones that hold every column and the strip.
size :: Gen (Int, Int)
size =
  (,)
    <$> Gen.choice [Gen.int (Range.linear 0 6), Gen.element [20, 24, 46, 122]]
    <*> Gen.choice [Gen.int (Range.linear 0 6), Gen.element [8, 24]]

-- | Terminals with room inside the margin for a column, a blank row and the
-- strip under it.
roomy :: Gen (Int, Int)
roomy = (,) <$> Gen.int (Range.linear 1 122) <*> Gen.int (Range.linear 3 24)

-- | A key press the browsing screen binds to nothing at all.
unbound :: Gen (Key, [Modifier])
unbound =
  Gen.filter (\(key, held) -> isNothing (command key held)) $
    (,)
      <$> Gen.choice
        [ Character <$> Gen.unicode
        , Gen.element [Enter, Escape, Backspace, UpArrow, DownArrow, LeftArrow, RightArrow]
        ]
      <*> Gen.subsequence [Ctrl, Shift]

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

walking :: Library IO -> Session -> [Command] -> IO Screen
walking held session = foldM (taking held session) start

-- | The screen one key press leaves behind. A key that ends browsing leaves
-- none, and for that the screen it was pressed on stands.
taking :: Library IO -> Session -> Screen -> Command -> IO Screen
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
-- clock. Every test here strikes its own beats, so none of them waits.
beaten :: Session -> Double -> Screen -> IO Screen
beaten session at = onBeat session (Moment at)

-- | The screen with the first song of Drukqs picked, its audio started, and
-- the strip caught up with it, which is where every test about the overlay
-- starts.
onDrukqs :: Standin -> Session -> IO Screen
onDrukqs standin session = do
  picked <- after session playingDrukqs
  begin standin
  beaten session 0 picked

-- | The screen with the first song of Drukqs just picked, its audio not yet
-- started, and the strip caught up with it.
loadingDrukqs :: Session -> IO Screen
loadingDrukqs session = after session playingDrukqs >>= beaten session 0

-- | The screen the first song of Drukqs failing this way leaves behind: the
-- backend says so, and the next beat takes it in.
breaking :: Failure -> Standin -> Session -> IO Screen
breaking failure standin session = do
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
onKeys = mconcat . take 1 . reverse . trail

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
