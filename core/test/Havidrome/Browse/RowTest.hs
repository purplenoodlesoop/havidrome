{- | What an item reads as on its line: the text an artist, an album or a song
gives its column, and the symbol the song playback is on carries.
-}
module Havidrome.Browse.RowTest (tests) where

import Havidrome.Audio.State (Motion (Paused, Running))
import Havidrome.Browse.Fixtures (album, artist, song)
import Havidrome.Browse.Row (Mark (Mark), Row (row), mark, marking)
import Havidrome.Check (example)
import Havidrome.Playback.Playing (Sound (Loading, Sounding))
import Havidrome.Subsonic.Types (SongId (SongId))
import Havidrome.Width qualified as Width
import Hedgehog (Group (Group), (/==), (===))

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
      ( "marking says what playback is doing with the song, one space before its name"
      , example do
          marked Loading === "  2 " <> mark Loading <> " Vordhosbn"
          marked (Sounding Running) === "  2 " <> mark (Sounding Running) <> " Vordhosbn"
          marked (Sounding Paused) === "  2 " <> mark (Sounding Paused) <> " Vordhosbn"
      )
    ,
      ( "the symbol is a different one for each of the three"
      , example do
          mark Loading /== mark (Sounding Running)
          mark (Sounding Running) /== mark (Sounding Paused)
          mark (Sounding Paused) /== mark Loading
      )
    ,
      ( "marking keeps the name in line with the unmarked rows around it, whichever symbol it is"
      , example
          ( fmap (Width.text . marked) sounds
              === fmap (const (Width.text (row vordhosbn))) sounds
          )
      )
    ,
      ( "marking leaves every other song as its row"
      , example do
          marking (Just (Mark (SongId "t") (Sounding Running))) vordhosbn === "  2   Vordhosbn"
          marking Nothing vordhosbn === "  2   Vordhosbn"
      )
    ]
 where
  vordhosbn = song "s" "Vordhosbn" 293 (Just 2)
  marked sound = marking (Just (Mark (SongId "s") sound)) vordhosbn
  sounds = [Loading, Sounding Running, Sounding Paused]
