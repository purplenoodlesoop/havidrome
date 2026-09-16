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
  , cells
  , symbol
  , line
  , row

    -- * The turn the loading symbol is on
  , Turn
  , turn
  , turns

    -- * The mark in the song list
  , Mark (..)
  , marking
  , mark
  ) where

import Data.Text as T (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Havidrome.Audio.State (Motion (Paused, Running))
import Havidrome.Browse.Strip (held)
import Havidrome.Divide (remainder)
import Havidrome.Moment (Moment (Moment))
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
the level under an artist or an album, the audio of a song — and which turn
of the loading symbol's round it is being said on, or, a song having its
audio, that the audio runs or is held.
-}
data Carried
  = Awaited Turn
  | Heard
  | Held
  deriving stock (Eq, Show)

-- | Everything a cell is ever asked to hold, which is a closed set.
cells :: [Carried]
cells = Heard : Held : fmap Awaited turns

{- | The symbol that is. Each takes the one column the cell has, so the names
in a column stay in line whichever of them a row carries. A song whose audio
is held carries the very character the bottom strip shows for a song held,
so that the list and the strip read as one thing said in two places.
-}
symbol :: Carried -> Text
symbol = \case
  Awaited at -> dot at
  Heard -> "▶"
  Held -> held

{- | Which turn of its round the loading symbol's dot is on. It is read off
the player's own clock rather than off the moment a row began waiting, so
two rows waiting at once are on the same turn however long each has waited.
-}
newtype Turn = Turn Int
  deriving stock (Eq, Ord, Show)

-- | The turns of a round, in the order the dot goes through them.
turns :: [Turn]
turns = fmap Turn [0 .. T.length dots - 1]

-- | The turn a moment on the player's clock is on.
turn :: Moment -> Turn
turn (Moment at) = case remainder (floor (at * rate)) (T.length dots) of
  -- There are dots for the one dot to go round, so there is a turn to be on.
  Nothing -> Turn 0
  Just on -> Turn on

{- | How many turns the dot takes a second, which is one every tenth of a
second.
-}
rate :: Double
rate = 10

{- | The round the dot goes: one dot going round a braille cell, a turn to a
character, back to the first once it has been round them all.
-}
dots :: Text
dots = "⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏"

{- | The character a turn of that round reads as, which is one column of
braille whichever turn it is.
-}
dot :: Turn -> Text
dot (Turn at) = T.take 1 (T.drop at dots)

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
marking :: Turn -> Maybe Mark -> Song -> Text
marking at on song = line (mark at <$> sounding) song
 where
  sounding = case on of
    Just marked | marked.song == song.id -> Just marked.sound
    _ -> Nothing

{- | What a song carries while playback is doing this with it: the awaited
symbol while it is loading, on the turn every row waiting at this moment is
on, which is the one an artist or an album carries while the level under it
is on its way, and one symbol each for its audio running and its audio held.
-}
mark :: Turn -> Sound -> Carried
mark at = \case
  Loading -> Awaited at
  Sounding Running -> Heard
  Sounding Paused -> Held

figure :: Int -> Maybe Int -> Text
figure width =
  maybe (T.replicate width " ") (T.justifyRight width ' ' . T.pack . show)
