{-# LANGUAGE LambdaCase #-}

-- | Where browsing stands: one list on screen at a time, artists above an
-- artist's albums above an album's songs.
--
-- A t'Browse' carries every level above the one on screen, each with the
-- selection it was left at, so that going back up finds a list exactly as it
-- was. Levels are only ever built from a 'Library', so a list on screen is
-- always one the server gave, in the order the server gave it.
module Havidrome.Browse
  ( -- * The three levels
    Browse (..)
  , Name (..)
  , Rows
  , atArtists

    -- * Moving through them
  , moveUp
  , moveDown
  , descend
  , ascend

    -- * Reading one
  , rows
  , selected
  ) where

import Brick.Widgets.List (List, list, listMoveBy, listSelectedElement)
import Data.Vector qualified as Vector
import Havidrome.Library (Library)
import Havidrome.Library qualified as Library
import Havidrome.Subsonic (Album (albumId), Artist (artistId), Song)

-- | The name brick knows a level's list by. One name per level, so no level
-- inherits another's scroll position.
data Name
  = ArtistList
  | AlbumList
  | SongList
  deriving stock (Eq, Ord, Show)

-- | One level's items, remembering which of them is selected.
type Rows a = List Name a

-- | The level on screen, and every level above it.
data Browse
  = -- | The artist list, which nothing sits above.
    AtArtists (Rows Artist)
  | -- | The selected artist's albums.
    AtAlbums (Rows Artist) (Rows Album)
  | -- | The selected album's songs.
    AtSongs (Rows Artist) (Rows Album) (Rows Song)
  deriving stock (Show)

-- | Browsing as it opens: the artist list, selection on the first artist.
atArtists :: [Artist] -> Browse
atArtists = AtArtists . rows ArtistList

-- | A level's items as a list brick can render, selection at the top.
rows :: Name -> [a] -> Rows a
rows name items = list name (Vector.fromList items) 1

-- | The item the selection is on, or nothing at all when the level is empty.
selected :: Rows a -> Maybe a
selected = fmap snd . listSelectedElement

-- | The selection one row up, or where it already is at the top of the list.
moveUp :: Browse -> Browse
moveUp = moveBy (-1)

-- | The selection one row down, or where it already is at the bottom.
moveDown :: Browse -> Browse
moveDown = moveBy 1

-- | Moving keeps the selection inside the list: at either end it stays put.
moveBy :: Int -> Browse -> Browse
moveBy distance = \case
  AtArtists artists -> AtArtists (listMoveBy distance artists)
  AtAlbums artists albums -> AtAlbums artists (listMoveBy distance albums)
  AtSongs artists albums songs -> AtSongs artists albums (listMoveBy distance songs)

-- | One level down, into the list the library holds under the selected item.
--
-- A song has no level below it — picking one starts playback, which is not
-- browsing's business — and an empty level has nothing selected to descend
-- into; both stay where they are, asking the library for nothing.
descend :: (Applicative f) => Library f -> Browse -> f Browse
descend library = \case
  AtArtists artists ->
    case selected artists of
      Nothing -> pure (AtArtists artists)
      Just artist ->
        AtAlbums artists . rows AlbumList <$> Library.albums library (artistId artist)
  AtAlbums artists albums ->
    case selected albums of
      Nothing -> pure (AtAlbums artists albums)
      Just album ->
        AtSongs artists albums . rows SongList <$> Library.songs library (albumId album)
  AtSongs artists albums songs -> pure (AtSongs artists albums songs)

-- | One level up, to the list it was descended from, still selecting the item
-- that was descended into. The artist list has nothing above it, so there this
-- does nothing.
ascend :: Browse -> Browse
ascend = \case
  AtArtists artists -> AtArtists artists
  AtAlbums artists _ -> AtArtists artists
  AtSongs artists albums _ -> AtAlbums artists albums
