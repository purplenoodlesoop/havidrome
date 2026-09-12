-- | What a level's list holds, one line to an item: the text an artist, an
-- album or a song reads as in its column, and the mark the song playback is
-- on carries in the song list.
--
-- The text is all a column needs of an item, so a screen draws a level
-- without knowing what is in it, and what a list says is read back without a
-- terminal.
module Havidrome.Browse.Row
  ( Row (..)
  , marking
  , mark
  ) where

import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Subsonic.Types (Album (..), Artist (..), Song (..), SongId)

-- | What one item of a level reads as. An artist is its name; an album carries
-- the year it is ordered by, a song the track number it is ordered by, each in
-- a column of its own, blank where the server gave none.
class Row a where
  row :: a -> Text

instance Row Artist where
  row artist = artist.name

instance Row Album where
  row album = figure 4 album.year <> "  " <> album.name

instance Row Song where
  row = marking Nothing

-- | What a song reads as in the song list: its track number, then a column of
-- its own for the mark, then its title, a space either side of the mark's
-- column. The column holds the mark on the song playback is on and is blank
-- on every other, so every title in the list starts in the same column.
--
-- Only a song has this: an album or an artist is never marked for the song
-- playing out of it.
marking :: Maybe SongId -> Song -> Text
marking on song = figure 3 song.track <> " " <> marker <> " " <> song.title
  where
    marker
      | on == Just song.id = mark
      | otherwise = " "

-- | The mark on the song playback is on.
mark :: Text
mark = "▶"

figure :: Int -> Maybe Int -> Text
figure width =
  maybe (T.replicate width " ") (T.justifyRight width ' ' . T.pack . show)
