-- | What an item reads as on its line: the text an artist, an album or a song
-- gives its column, and the mark the song playback is on carries.
module Havidrome.Browse.RowTest (tests) where

import Data.Text qualified as Text
import Havidrome.Browse.Fixtures (album, artist, song)
import Havidrome.Browse.Row (Row (row), mark, marking)
import Havidrome.Check (example)
import Havidrome.Subsonic.Types (SongId (SongId))
import Hedgehog (Group (Group), (===))

tests :: Group
tests =
  Group
    "Havidrome.Browse.Row"
    [
      ( "row shows an artist by name"
      , example (row (artist "a" "Aphex Twin") === "Aphex Twin")
      )
    ,
      ( "row shows an album's year before its name"
      , example (row (album "b" "Drukqs" (Just 2001)) === "2001  Drukqs")
      )
    ,
      ( "row leaves the column blank for an album the server gave no year"
      , example (row (album "b" "Sketches" Nothing) === "      Sketches")
      )
    ,
      ( "row shows a song's track number before its title, the mark's column blank between them"
      , example (row (song "s" "Vordhosbn" 293 (Just 2)) === "  2   Vordhosbn")
      )
    ,
      ( "row leaves the column blank for a song the server gave no track number"
      , example (row (song "s" "Btoum Roumada" 96 Nothing) === "      Btoum Roumada")
      )
    ,
      ( "marking puts the mark and one space before the name of the song playback is on"
      , example (marking (Just (SongId "s")) vordhosbn === "  2 " <> mark <> " Vordhosbn")
      )
    ,
      ( "marking keeps the name in line with the unmarked rows around it"
      , example
          ( Text.length (marking (Just (SongId "s")) vordhosbn)
              === Text.length (row vordhosbn)
          )
      )
    ,
      ( "marking leaves every other song as its row"
      , example do
          marking (Just (SongId "t")) vordhosbn === "  2   Vordhosbn"
          marking Nothing vordhosbn === "  2   Vordhosbn"
      )
    ]
  where
    vordhosbn = song "s" "Vordhosbn" 293 (Just 2)
