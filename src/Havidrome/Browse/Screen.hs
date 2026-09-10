{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The browsing screen: the level's list under its title, the keys that move
-- through it, and the brick application that puts the two together.
module Havidrome.Browse.Screen
  ( -- * The screen
    Screen (..)
  , opening
  , application

    -- * The keys
  , Command (..)
  , command
  , step

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
  , BrickEvent (VtyEvent)
  , EventM
  , Padding (Max)
  , Widget
  , attrMap
  , attrName
  , emptyWidget
  , halt
  , neverShowCursor
  , padRight
  , txt
  , vBox
  , withAttr
  )
import Brick.Widgets.List (listSelectedFocusedAttr, renderList)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Havidrome.Browse
  ( Browse (AtAlbums, AtArtists, AtSongs)
  , Name
  , atArtists
  , selected
  )
import Havidrome.Browse qualified as Browse
import Havidrome.Library (Library)
import Havidrome.Subsonic
  ( Album (albumName, albumYear)
  , Artist (artistName)
  , Song (songTitle, songTrack)
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
  deriving stock (Eq, Show)

-- | The key map: arrows and @j@\/@k@ move, Enter descends, Esc goes back, @q@
-- quits. Every other key does nothing.
command :: Vty.Key -> [Vty.Modifier] -> Maybe Command
command key modifiers = case (key, modifiers) of
  (Vty.KUp, []) -> Just MoveUp
  (Vty.KChar 'k', []) -> Just MoveUp
  (Vty.KDown, []) -> Just MoveDown
  (Vty.KChar 'j', []) -> Just MoveDown
  (Vty.KEnter, []) -> Just Descend
  (Vty.KEsc, []) -> Just Ascend
  (Vty.KChar 'q', []) -> Just Quit
  _ -> Nothing


-- | The screen a command leaves behind, or nothing at all when the command was
-- to leave the player. Descending is the one command that asks the library
-- anything, and a library that will not answer leaves the level where it is,
-- with the reason in the bottom strip.
step ::
  (Monad f) =>
  Library (ExceptT SubsonicError f) ->
  Command ->
  Screen ->
  f (Maybe Screen)
step library instruction screen = case instruction of
  Quit -> pure Nothing
  MoveUp -> here Browse.moveUp
  MoveDown -> here Browse.moveDown
  Ascend -> here Browse.ascend
  Descend -> do
    descended <- runExceptT (Browse.descend library (browse screen))
    pure . Just $ case descended of
      Left failure -> quiet {trouble = Just (explain failure)}
      Right level -> quiet {browse = level}
  where
    -- Whatever a key press does, it first clears what went wrong before it.
    quiet = screen {trouble = Nothing}
    here move = pure (Just quiet {browse = move (browse screen)})

-- | The player, browsing the library it is given until @q@.
application :: Library (ExceptT SubsonicError IO) -> App Screen e Name
application library =
  App
    { appDraw = draw
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle library
    , appStartEvent = pure ()
    , appAttrMap = const theme
    }

handle :: Library (ExceptT SubsonicError IO) -> BrickEvent Name e -> EventM Name Screen ()
handle library = \case
  VtyEvent (Vty.EvKey key modifiers) ->
    case command key modifiers of
      Nothing -> pure ()
      Just instruction -> do
        screen <- get
        stepped <- liftIO (step library instruction screen)
        maybe halt put stepped
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
