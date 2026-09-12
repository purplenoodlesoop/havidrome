-- | Where browsing stands: artists, then an artist's albums, then an album's
-- songs, the deepest of them the level being browsed.
--
-- A t'Browse' carries the level being browsed and every level it was
-- descended from, each with the selection it was left at — the item picked in
-- it — so that the levels above can be shown beside it and going back up finds
-- a list exactly as it was. Levels are only ever built from a 'Library', so a
-- list on screen is always one the server gave, in the order the server gave
-- it.
module Havidrome.Browse
  ( -- * The three levels
    Browse (..)
  , Rows (..)
  , atArtists

    -- * Moving through them
  , moveUp
  , moveDown
  , descend
  , ascend

    -- * Reading one
  , rows
  , selected
  , picked
  ) where

import Data.Ord (clamp)
import Havidrome.Library (Library (..))
import Havidrome.Subsonic.Types (Album (..), Artist (..), Song, SubsonicError)

-- | One level's items, remembering which of them is selected: the item at
-- 'place', which is inside the list whenever there is one to be inside.
data Rows a = Rows
  { items :: [a]
  , place :: Int
  }
  deriving stock (Eq, Show)

-- | The level being browsed, and every level above it.
data Browse
  = -- | The artist list, which nothing sits above.
    AtArtists (Rows Artist)
  | -- | The selected artist's albums.
    AtAlbums (Rows Artist) (Rows Album)
  | -- | The selected album's songs.
    AtSongs (Rows Artist) (Rows Album) (Rows Song)
  deriving stock (Eq, Show)

-- | Browsing as it opens: the artist list, selection on the first artist.
atArtists :: [Artist] -> Browse
atArtists = AtArtists . rows

-- | A level's items, selection at the top.
rows :: [a] -> Rows a
rows items = Rows {items, place = 0}

-- | The item the selection is on, or nothing at all when the level is empty.
selected :: Rows a -> Maybe a
selected level = case drop level.place level.items of
  [] -> Nothing
  item : _ -> Just item

-- | What Enter on a song picks: the album whose songs are on screen, in album
-- order, and the song of it the selection is on. Playing them is playback's
-- business, so browsing only says which they are.
--
-- The two levels above a song have no song to pick, and an album with no songs
-- has nothing selected in it; neither picks anything.
picked :: Browse -> Maybe ([Song], Song)
picked = \case
  AtArtists _ -> Nothing
  AtAlbums _ _ -> Nothing
  AtSongs _ _ songs -> (,) songs.items <$> selected songs

-- | The selection one row up, or where it already is at the top of the list.
moveUp :: Browse -> Browse
moveUp = moveBy (-1)

-- | The selection one row down, or where it already is at the bottom.
moveDown :: Browse -> Browse
moveDown = moveBy 1

-- | Moving keeps the selection inside the list: at either end it stays put,
-- and an empty level has nowhere for it to go.
moveBy :: Int -> Browse -> Browse
moveBy distance = \case
  AtArtists artists -> AtArtists (shift distance artists)
  AtAlbums artists albums -> AtAlbums artists (shift distance albums)
  AtSongs artists albums songs -> AtSongs artists albums (shift distance songs)

-- | One level's selection moved this many rows, and no further than the list
-- it is in reaches.
shift :: Int -> Rows a -> Rows a
shift distance level =
  level {place = clamp (0, max 0 (length level.items - 1)) (level.place + distance)}

-- | One level down, into the list the library holds under the selected item,
-- or the failure that stopped the library answering it. A level that will not
-- open is no level at all: the failure comes back whole, for the caller to say
-- so, rather than a half-filled list reaching the screen.
--
-- A song has no level below it — picking one starts playback, which is not
-- browsing's business — and an empty level has nothing selected to descend
-- into; both stay where they are, asking the library for nothing, so neither
-- can fail.
descend :: (Applicative f) => Library f -> Browse -> f (Either SubsonicError Browse)
descend library = \case
  AtArtists artists ->
    case selected artists of
      Nothing -> stays (AtArtists artists)
      Just artist -> fmap (AtAlbums artists . rows) <$> library.albums artist.id
  AtAlbums artists albums ->
    case selected albums of
      Nothing -> stays (AtAlbums artists albums)
      Just album -> fmap (AtSongs artists albums . rows) <$> library.songs album.id
  AtSongs artists albums songs -> stays (AtSongs artists albums songs)
  where
    stays = pure . Right

-- | One level up, to the list it was descended from, still selecting the item
-- that was descended into. The artist list has nothing above it, so there this
-- does nothing.
ascend :: Browse -> Browse
ascend = \case
  AtArtists artists -> AtArtists artists
  AtAlbums artists _ -> AtArtists artists
  AtSongs artists albums _ -> AtAlbums artists albums
