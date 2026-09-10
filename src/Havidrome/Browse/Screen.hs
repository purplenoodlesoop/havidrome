{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The browsing screen: the level's list under its title, the keys that move
-- through it, and the brick application that puts the two together.
--
-- The screen is also where a song is picked: Enter on one hands its album to
-- the playback session, and the audio runs on underneath while the lists go on
-- being browsed.
module Havidrome.Browse.Screen
  ( -- * The screen
    Screen (..)
  , opening
  , browsing
  , application

    -- * The keys
  , Command (..)
  , command
  , step

    -- * The beat the audio is carried on
  , Beat (..)
  , onBeat

    -- * What it looks like
  , draw
  , theme
  , title
  , row
  , explain
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (AppEvent, VtyEvent)
  , EventM
  , Padding (Max)
  , Widget
  , attrMap
  , attrName
  , customMainWithDefaultVty
  , emptyWidget
  , halt
  , neverShowCursor
  , padRight
  , txt
  , vBox
  , withAttr
  )
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.List (listSelectedFocusedAttr, renderList)
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Havidrome.Audio (Failure)
import Havidrome.Browse
  ( Browse (AtAlbums, AtArtists, AtSongs)
  , Name
  , atArtists
  , picked
  , selected
  )
import Havidrome.Browse qualified as Browse
import Havidrome.Library (Library)
import Havidrome.Playback (Session, startingAt)
import Havidrome.Playback qualified as Playback
import Havidrome.Subsonic
  ( Album (albumName, albumYear)
  , Artist (artistName)
  , Song (songId, songTitle, songTrack)
  , SubsonicError (AuthRejected, MalformedResponse, NetworkFailure, ServerFailure)
  )

-- | Everything on screen: the level being browsed, and whatever went wrong
-- last, which sits in the strip along the bottom until the next key press.
data Screen = Screen
  { browse :: Browse
  , trouble :: Maybe Text
  }
  deriving stock (Show)

-- | The screen a run opens on: the artist list, nothing wrong yet.
opening :: [Artist] -> Screen
opening artists = Screen {browse = atArtists artists, trouble = Nothing}

-- | What a key press means. Nothing else on the browsing screen does anything.
data Command
  = -- | Move the selection one row up.
    MoveUp
  | -- | Move the selection one row down.
    MoveDown
  | -- | Into the selected item's list.
    Descend
  | -- | Back to the list above.
    Ascend
  | -- | Leave the player.
    Quit
  | -- | Hold the playing audio where it is, or let a held one run on again.
    PauseOrResume
  | -- | Play the next song of the album being played.
    NextSong
  | -- | Play the previous song of the album being played.
    PreviousSong
  | -- | Move the song being played this many seconds, forwards or back.
    Seek Int
  deriving stock (Eq, Show)

-- | The key map. Arrows and @j@\/@k@ move, Enter descends, Esc goes back, and
-- Ctrl+C leaves the player; @space@, @n@, @p@ and left\/right reach past the
-- lists to the song being played. Every other key does nothing: no letter
-- quits, @q@ included, so it is as inert here as any other unbound key.
command :: Vty.Key -> [Vty.Modifier] -> Maybe Command
command key modifiers = case (key, modifiers) of
  (Vty.KUp, []) -> Just MoveUp
  (Vty.KChar 'k', []) -> Just MoveUp
  (Vty.KDown, []) -> Just MoveDown
  (Vty.KChar 'j', []) -> Just MoveDown
  (Vty.KEnter, []) -> Just Descend
  (Vty.KEsc, []) -> Just Ascend
  (Vty.KChar 'c', [Vty.MCtrl]) -> Just Quit
  (Vty.KChar ' ', []) -> Just PauseOrResume
  (Vty.KChar 'n', []) -> Just NextSong
  (Vty.KChar 'p', []) -> Just PreviousSong
  (Vty.KRight, []) -> Just (Seek nudge)
  (Vty.KLeft, []) -> Just (Seek (-nudge))
  (Vty.KRight, [Vty.MShift]) -> Just (Seek stride)
  (Vty.KLeft, [Vty.MShift]) -> Just (Seek (-stride))
  _ -> Nothing

-- | How far a seek moves the song being played: the arrows alone nudge it,
-- and shift held with them strides.
nudge, stride :: Int
nudge = 5
stride = 30

-- | The screen a command leaves behind, or nothing at all when the command was
-- to leave the player.
--
-- Descending on a song is not descending at all: there is no level under a
-- song, so Enter there hands that song's album to the session, which plays it
-- from that song in place of whatever was playing. The list stays exactly
-- where it is, and so does every list above it — picking a song moves the
-- audio and nothing else.
--
-- Descending anywhere else is the one command that asks the library anything,
-- and a library that will not answer leaves the level where it is, with the
-- reason in the bottom strip. Moving and going back up ask nothing and touch
-- no audio, which is what keeps browsing live under a playing song.
--
-- The last four go the other way about: they reach the song being played and
-- leave the level and the selection exactly as they were, at whichever of the
-- three levels they were pressed. With nothing playing the session has no song
-- to hold, move through or move past, and so they do nothing at all.
step ::
  Library (ExceptT SubsonicError IO) ->
  Session ->
  Command ->
  Screen ->
  IO (Maybe Screen)
step library session instruction screen = case instruction of
  Quit -> Playback.stop session >> pure Nothing
  MoveUp -> here Browse.moveUp
  MoveDown -> here Browse.moveDown
  Ascend -> here Browse.ascend
  PauseOrResume -> toAudio (Playback.togglePause session)
  NextSong -> toAudio (Playback.next session)
  PreviousSong -> toAudio (Playback.previous session)
  Seek by -> toAudio (Playback.seekBy session by)
  Descend -> case picked (browse screen) of
    Just (album, song) -> do
      traverse_ (Playback.start session) (startingAt album (songId song))
      pure (Just quiet)
    Nothing -> do
      descended <- runExceptT (Browse.descend library (browse screen))
      pure . Just $ case descended of
        Left failure -> quiet {trouble = Just (explain failure)}
        Right level -> quiet {browse = level}
  where
    -- Whatever a key press does, it first clears what went wrong before it.
    quiet = screen {trouble = Nothing}
    here move = pure (Just quiet {browse = move (browse screen)})
    toAudio act = act >> pure (Just quiet)

-- | The beat the player hears between key presses. There is nothing to it: it
-- is the moment on which what the audio has done is taken in.
data Beat = Beat
  deriving stock (Eq, Show)

-- | What the player takes in on a beat: everything the audio has done since
-- the last one. This is where an album carries itself — a song running out
-- starts the next song of that album, with no key pressed.
--
-- It hands back the failures the bottom strip is to show; putting them there
-- is the now-playing overlay's, which is not built yet.
onBeat :: Session -> IO [Failure]
onBeat = Playback.attend

-- | Hands the terminal to the browsing screen, and takes it back when the
-- player is left. The beat runs for exactly as long as the screen is up.
browsing :: Library (ExceptT SubsonicError IO) -> Session -> Screen -> IO ()
browsing library session screen = do
  beats <- newBChan 1
  bracket (forkIO (beating beats)) killThread $ \_ ->
    void (customMainWithDefaultVty (Just beats) (application library session) screen)

-- | A beat, then the next, for as long as it is left running.
beating :: BChan Beat -> IO ()
beating beats = forever (writeBChan beats Beat >> threadDelay interval)

-- | How long a beat lasts, in microseconds: short enough that one song follows
-- another without a silence to hear, long enough that the player is idle
-- between beats.
interval :: Int
interval = 100_000

-- | The player, browsing the library it is given until Ctrl+C, over the
-- session that plays what is picked in it.
application :: Library (ExceptT SubsonicError IO) -> Session -> App Screen Beat Name
application library session =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle library session
    , appStartEvent = pure ()
    , appAttrMap = const theme
    }

handle ::
  Library (ExceptT SubsonicError IO) ->
  Session ->
  BrickEvent Name Beat ->
  EventM Name Screen ()
handle library session = \case
  VtyEvent (Vty.EvKey key modifiers) ->
    case command key modifiers of
      Nothing -> pure ()
      Just instruction -> do
        screen <- get
        stepped <- liftIO (step library session instruction screen)
        maybe halt put stepped
  AppEvent Beat -> void (liftIO (onBeat session))
  _ -> pure ()

-- | The whole screen: the title of the level, its list under it filling
-- everything left, and the bottom strip when there is something to say.
draw :: Screen -> [Widget Name]
draw screen =
  [ vBox
      [ withAttr titleAttribute (line (title (browse screen)))
      , level (browse screen)
      , maybe emptyWidget (withAttr troubleAttribute . line) (trouble screen)
      ]
  ]
  where
    level = \case
      AtArtists artists -> renderList (const (line . row)) True artists
      AtAlbums _ albums -> renderList (const (line . row)) True albums
      AtSongs _ _ songs -> renderList (const (line . row)) True songs

-- | A row of text across the full width, so that highlighting one covers the
-- line and not just its letters.
line :: Text -> Widget Name
line = padRight Max . txt

-- | Where in the library the level on screen is: the artist list says so, and
-- the lists under it are named by what was descended into.
title :: Browse -> Text
title = \case
  AtArtists _ -> "Artists"
  AtAlbums artists _ -> named artistName artists
  AtSongs artists albums _ -> named artistName artists <> " — " <> named albumName albums
  where
    named name = maybe "" name . selected

-- | What one item of a level reads as. An artist is its name; an album carries
-- the year it is ordered by, a song the track number it is ordered by, each in
-- a column of its own, blank where the server gave none.
class Row a where
  row :: a -> Text

instance Row Artist where
  row = artistName

instance Row Album where
  row album = column 4 (albumYear album) <> "  " <> albumName album

instance Row Song where
  row song = column 3 (songTrack song) <> "  " <> songTitle song

column :: Int -> Maybe Int -> Text
column width =
  maybe (Text.replicate width " ") (Text.justifyRight width ' ' . Text.pack . show)

-- | What went wrong, in a sentence for the bottom strip.
explain :: SubsonicError -> Text
explain = \case
  NetworkFailure reason -> "The server could not be reached: " <> reason
  AuthRejected reason -> "The server refused these credentials: " <> reason
  ServerFailure code reason ->
    "The server answered with an error (" <> Text.pack (show code) <> "): " <> reason
  MalformedResponse reason -> "The server's answer could not be read: " <> reason

-- | The selected row is the one in reverse video; the title is bold and the
-- bottom strip red. Everything else is the terminal's own colours.
theme :: AttrMap
theme =
  attrMap
    Vty.defAttr
    [ (listSelectedFocusedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
    , (titleAttribute, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (troubleAttribute, Vty.defAttr `Vty.withForeColor` Vty.red)
    ]

titleAttribute, troubleAttribute :: AttrName
titleAttribute = attrName "title"
troubleAttribute = attrName "trouble"
