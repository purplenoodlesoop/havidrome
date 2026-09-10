{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The browsing screen: the level's list under its title, the strip along the
-- bottom, the keys that move through it, and the brick application that puts
-- them together.
--
-- The screen is also where a song is picked: Enter on one hands its album to
-- the playback session, and the audio runs on underneath while the lists go on
-- being browsed. What that audio is doing is what the bottom strip says, and
-- the song it is on carries a mark of its own in the song list, wherever the
-- selection is.
--
-- Browsing ends in exactly one of two ways, and the audio is stopped either
-- way: the player was left, or the account was. The second hands the run back
-- to the login screen, where another account can be entered.
module Havidrome.Browse.Screen
  ( -- * The screen
    Screen (..)
  , opening
  , browsing
  , application

    -- * The keys
  , Command (..)
  , command
  , Ending (..)
  , step

    -- * The beat the audio is carried on
  , Beat (..)
  , onBeat

    -- * What it looks like
  , draw
  , theme
  , title
  , row
  , marking
  , mark
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
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Foldable (traverse_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Havidrome.Browse
  ( Browse (AtAlbums, AtArtists, AtSongs)
  , Name
  , atArtists
  , picked
  , selected
  )
import Havidrome.Browse qualified as Browse
import Havidrome.Browse.Strip (Moment, Showing (Overlay, Wrong), Strip)
import Havidrome.Browse.Strip qualified as Strip
import Havidrome.Library (Library)
import Havidrome.Playback (Playing (playingSong), Session, startingAt)
import Havidrome.Playback qualified as Playback
import Havidrome.Subsonic
  ( Album (albumName, albumYear)
  , Artist (artistName)
  , Song (songId, songTitle, songTrack)
  , SongId
  , SubsonicError
  , explain
  )

-- | Everything on screen: the level being browsed, the strip along the bottom
-- that says what the audio is doing and what last went wrong, the song the
-- audio is on, and how browsing ended, once it has ended.
data Screen = Screen
  { browse :: Browse
  , strip :: Strip
  , marked :: Maybe SongId
  -- ^ The song playback is on, which carries the mark in whichever song list
  -- it is in; nothing while nothing is playing.
  , ending :: Maybe Ending
  }
  deriving stock (Show)

-- | The screen a run opens on: the artist list, nothing playing, nothing
-- wrong yet, and browsing still going on.
opening :: [Artist] -> Screen
opening artists =
  Screen
    { browse = atArtists artists
    , strip = Strip.quiet
    , marked = Nothing
    , ending = Nothing
    }

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
    Leave
  | -- | Leave the account, and go back to the login screen for another.
    LogOut
  | -- | Hold the playing audio where it is, or let a held one run on again.
    PauseOrResume
  | -- | Play the next song of the album being played.
    NextSong
  | -- | Play the previous song of the album being played.
    PreviousSong
  | -- | Move the song being played this many seconds, forwards or back.
    Seek Int
  deriving stock (Eq, Show)

-- | The key map. Arrows and @j@\/@k@ move, Enter descends, Esc goes back,
-- Ctrl+C leaves the player and @l@ leaves the account; @space@, @n@, @p@ and
-- left\/right reach past the lists to the song being played. Every other key
-- does nothing: no letter quits, @q@ included, so it is as inert here as any
-- other unbound key.
command :: Vty.Key -> [Vty.Modifier] -> Maybe Command
command key modifiers = case (key, modifiers) of
  (Vty.KUp, []) -> Just MoveUp
  (Vty.KChar 'k', []) -> Just MoveUp
  (Vty.KDown, []) -> Just MoveDown
  (Vty.KChar 'j', []) -> Just MoveDown
  (Vty.KEnter, []) -> Just Descend
  (Vty.KEsc, []) -> Just Ascend
  (Vty.KChar 'c', [Vty.MCtrl]) -> Just Leave
  (Vty.KChar 'l', []) -> Just LogOut
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

-- | How browsing ended.
data Ending
  = -- | The player was left.
    Quit
  | -- | The account was left. What comes of that is not browsing's business:
    -- it only says that this is how the screen ended.
    LoggedOut
  deriving stock (Eq, Show)

-- | The screen a command leaves behind, or the ending it brought about.
--
-- Leaving the player and logging out are the two commands that end browsing,
-- and both stop the audio on the way: neither leaves a song playing behind a
-- screen that is gone.
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
--
-- Whichever it was, the screen that stays asks the session afterwards which
-- song it is on, so the mark is on a picked song from the key press that
-- picked it — while it is still loading — and follows @n@ and @p@ at once.
step ::
  Library (ExceptT SubsonicError IO) ->
  Session ->
  Command ->
  Screen ->
  IO (Either Ending Screen)
step library session instruction screen = case instruction of
  Leave -> ends Quit
  LogOut -> ends LoggedOut
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
      stays taken
    Nothing -> do
      descended <- runExceptT (Browse.descend library (browse screen))
      stays $ case descended of
        Left failure -> taken {strip = Strip.wrong (explain failure) (strip taken)}
        Right level -> taken {browse = level}
  where
    -- Whatever a key press does, the strip hears about it first.
    taken = screen {strip = Strip.pressed (strip screen)}
    stays next = do
      on <- Playback.nowPlaying session
      pure (Right next {marked = markOf on})
    here move = stays taken {browse = move (browse screen)}
    toAudio act = act >> stays taken
    ends ended = Playback.stop session >> pure (Left ended)

-- | The beat the player hears between key presses: the moment it happened at,
-- which is both when what the audio has done is taken in and the clock a line
-- with a few seconds to live is measured against.
newtype Beat = Beat Moment
  deriving stock (Eq, Show)

-- | What the player takes in on a beat: everything the audio has done since
-- the last one, and how far it has come into the song it is on.
--
-- This is where an album carries itself — a song running out starts the next
-- song of that album, with no key pressed — and where the bottom strip is kept
-- true: the overlay's elapsed time moves on with the audio, and a failure the
-- audio reports lands on the strip in place of the overlay's contents. The mark
-- moves with the audio too — onto the song an album moved on to or a skip
-- landed on, and off every song once the playing has ended.
onBeat :: Session -> Moment -> Screen -> IO Screen
onBeat session at screen = do
  failures <- Playback.attend session
  playing <- Playback.nowPlaying session
  pure
    screen
      { strip = Strip.beat at playing failures (strip screen)
      , marked = markOf playing
      }

-- | The song that carries the mark: the one being played, if any is.
markOf :: Maybe Playing -> Maybe SongId
markOf = fmap (songId . playingSong)

-- | Hands the terminal to the browsing screen, takes it back when browsing
-- ends, and says how it ended. The beat runs for exactly as long as the screen
-- is up.
browsing :: Library (ExceptT SubsonicError IO) -> Session -> Screen -> IO Ending
browsing library session screen = do
  beats <- newBChan 1
  bracket (forkIO (beating beats)) killThread $ \_ -> do
    (final, vty) <- customMainWithDefaultVty (Just beats) (application library session) screen
    -- brick hands back the terminal it was driving rather than putting it
    -- down, so that one screen can hand it to the next. This one hands it to
    -- nobody: the login screen a logout goes to takes a terminal of its own,
    -- and a run that ends here leaves the terminal as it found it.
    Vty.shutdown vty
    -- Only the two commands that end browsing take the screen down, and each
    -- writes down which of them it was; a screen that is gone for any other
    -- reason is one the player was left at.
    pure (fromMaybe Quit (ending final))

-- | A beat, then the next, for as long as it is left running.
beating :: BChan Beat -> IO ()
beating beats = forever $ do
  at <- Strip.moment
  writeBChan beats (Beat at)
  threadDelay interval

-- | How long a beat lasts, in microseconds: short enough that one song follows
-- another without a silence to hear, long enough that the player is idle
-- between beats.
interval :: Int
interval = 100_000

-- | The player, browsing the library it is given until it is left, over the
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
        case stepped of
          Left ended -> put screen {ending = Just ended} >> halt
          Right stepping -> put stepping
  AppEvent (Beat at) -> get >>= liftIO . onBeat session at >>= put
  _ -> pure ()

-- | The whole screen: the title of the level, its list under it filling
-- everything left, and the one-line strip along the bottom whenever it has
-- anything on it.
draw :: Screen -> [Widget Name]
draw screen =
  [ vBox
      [ withAttr titleAttribute (line (title (browse screen)))
      , level (browse screen)
      , maybe emptyWidget bottom (Strip.showing (strip screen))
      ]
  ]
  where
    bottom = \case
      Wrong said -> withAttr troubleAttribute (line said)
      Overlay said -> withAttr overlayAttribute (line said)
    level = \case
      AtArtists artists -> renderList (const (line . row)) True artists
      AtAlbums _ albums -> renderList (const (line . row)) True albums
      AtSongs _ _ songs -> renderList (const (line . marking (marked screen))) True songs

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

-- | What a song reads as in the song list: its row, and when it is the song
-- playback is on, the mark immediately before its name. The mark takes the
-- place of the second of the two spaces there, so a marked name stays in line
-- with the names above and below it.
--
-- Only a song has this: an album or an artist is never marked for the song
-- playing out of it.
marking :: Maybe SongId -> Song -> Text
marking on song
  | on == Just (songId song) = column 3 (songTrack song) <> " " <> mark <> songTitle song
  | otherwise = row song

-- | The mark on the song playback is on.
mark :: Text
mark = "▶"

column :: Int -> Maybe Int -> Text
column width =
  maybe (Text.replicate width " ") (Text.justifyRight width ' ' . Text.pack . show)

-- | The selected row is the one in reverse video; the title line and the
-- now-playing overlay are bold, and a reason in the bottom strip is red.
-- Everything else is the terminal's own colours.
theme :: AttrMap
theme =
  attrMap
    Vty.defAttr
    [ (listSelectedFocusedAttr, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
    , (titleAttribute, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (overlayAttribute, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (troubleAttribute, Vty.defAttr `Vty.withForeColor` Vty.red)
    ]

titleAttribute, overlayAttribute, troubleAttribute :: AttrName
titleAttribute = attrName "title"
overlayAttribute = attrName "overlay"
troubleAttribute = attrName "trouble"
