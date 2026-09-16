{- | What a level's list holds, one line to an item: the text an artist, an
album or a song reads as in its column, and the symbol it carries in the one
cell every row keeps for one.

The text is all a column needs of an item, so a screen draws a level
without knowing what is in it, and what a list says is read back without a
terminal.
-}
module Havidrome.Browse.Row
  ( -- * A row's line
    Row (..)
  , Carried (..)
  , symbol
  , line
  , row

    -- * The mark in the song list
  , Mark (..)
  , marking
  , mark
  ) where

import Data.Text as T (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Havidrome.Audio.State (Motion (Paused, Running))
import Havidrome.Playback.Playing (Sound (Loading, Sounding))
import Havidrome.Subsonic.Types (Album (..), Artist (..), Song (..), SongId)

{- | The two parts of an item's line, either side of its cell: what its level
is ordered by, and what the item is called. An album is ordered by the year
and a song by the track number, each in a column of its own, blank where the
server gave none; an artist is ordered by nothing but its own name.
-}
class Row a where
  ordered :: a -> Text
  called :: a -> Text

instance Row Artist where
  ordered _ = ""
  called artist = artist.name

instance Row Album where
  ordered album = figure 4 album.year
  called album = album.name

instance Row Song where
  ordered song = figure 3 song.track
  called song = song.title

{- | What a row carries in its cell: that what it was asked for is on its way —
the level under an artist or an album, the audio of a song — or, a song
having its audio, that the audio runs or is held.
-}
data Carried
  = Awaited
  | Heard
  | Held
  deriving stock (Bounded, Enum, Eq, Show)

{- | The symbol that is. Each takes the one column the cell has, so the names
in a column stay in line whichever of them a row carries.
-}
symbol :: Carried -> Text
symbol = \case
  Awaited -> "⋯"
  Heard -> "▶"
  Held -> "‖"

{- | An item's line: what its level is ordered by, then the one cell every row
keeps for a symbol, a space on either side of it, then what the item is
called. A row carrying nothing leaves that cell blank, so every name in a
column starts in the same place whether or not its row carries a symbol.
-}
line :: (Row a) => Maybe Carried -> a -> Text
line carried item =
  ordered item <> " " <> maybe " " symbol carried <> " " <> called item

-- | The line of a row carrying nothing, which is what most rows are.
row :: (Row a) => a -> Text
row = line Nothing

{- | The mark in a song list: which song playback is on, and what it is doing
with it, which is what the symbol on that song says.
-}
data Mark = Mark
  { song :: SongId
  , sound :: Sound
  }
  deriving stock (Eq, Generic, Show)

{- | What a song reads as in the song list: its line, carrying the mark's
symbol if the mark is on it and nothing if it is not.

Only a song is ever marked this way: an album or an artist is never marked
for the song playing out of it, and carries a symbol only while the level
under it is on its way.
-}
marking :: Maybe Mark -> Song -> Text
marking on song = line (mark <$> sounding) song
 where
  sounding = case on of
    Just carried | carried.song == song.id -> Just carried.sound
    _ -> Nothing

{- | What a song carries while playback is doing this with it: the awaited
symbol while it is loading, which is the one an artist or an album carries
while the level under it is on its way, and one symbol each for its audio
running and its audio held.
-}
mark :: Sound -> Carried
mark = \case
  Loading -> Awaited
  Sounding Running -> Heard
  Sounding Paused -> Held

figure :: Int -> Maybe Int -> Text
figure width =
  maybe (T.replicate width " ") (T.justifyRight width ' ' . T.pack . show)
