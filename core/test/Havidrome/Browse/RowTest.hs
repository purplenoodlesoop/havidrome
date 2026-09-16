{- | What an item reads as on its line: the text an artist, an album or a song
gives its column, and the symbol it carries in the cell before its name.
-}
module Havidrome.Browse.RowTest (tests) where

import Data.Text as T (Text)
import Havidrome.Audio.State (Motion (Paused, Running))
import Havidrome.Browse.Fixtures (album, artist, song)
import Havidrome.Browse.Row
  ( Carried (Awaited, Heard, Held)
  , Mark (Mark)
  , line
  , mark
  , marking
  , row
  , symbol
  )
import Havidrome.Check (Checks, example)
import Havidrome.Playback.Playing (Sound (Loading, Sounding))
import Havidrome.Subsonic.Types (SongId (SongId))
import Havidrome.Width qualified as Width
import Hedgehog (Group (Group), (/==), (===))

tests :: Group
tests = Group "Havidrome.Browse.Row" (rows <> cells <> marks)

-- | What each of the three reads as with its cell empty.
rows :: Checks
rows =
  [
    ( "row shows an artist by name, the cell before it blank"
    , example (row (artist "a" "Aphex Twin") === "   Aphex Twin")
    )
  ,
    ( "row shows an album's year before its name, the cell blank between them"
    , example (row (album "b" "Drukqs" (Just 2001)) === "2001   Drukqs")
    )
  ,
    ( "row leaves the year's columns blank for an album the server gave no year"
    , example (row (album "b" "Sketches" Nothing) === "       Sketches")
    )
  ,
    ( "row shows a song's track number before its title, the cell blank between them"
    , example (row (song "s" "Vordhosbn" 293 (Just 2)) === "  2   Vordhosbn")
    )
  ,
    ( "row leaves the track's columns blank for a song the server gave no track number"
    , example (row (song "s" "Btoum Roumada" 96 Nothing) === "      Btoum Roumada")
    )
  ]

-- | The one cell every row keeps for a symbol, whatever level the row is on.
cells :: Checks
cells =
  [
    ( "line puts the symbol one space before the name, in the cell row leaves blank"
    , example do
        line (Just Awaited) (artist "a" "Aphex Twin") === " " <> symbol Awaited <> " Aphex Twin"
        line (Just Awaited) (album "b" "Drukqs" (Just 2001))
          === "2001 " <> symbol Awaited <> " Drukqs"
        line (Just Awaited) (song "s" "Vordhosbn" 293 (Just 2))
          === "  2 " <> symbol Awaited <> " Vordhosbn"
    )
  ,
    ( "line keeps every name where row leaves it, whichever symbol the cell carries"
    , example do
        widths (\carried -> line carried (artist "a" "Aphex Twin")) === widths (const "   Aphex Twin")
        widths (\carried -> line carried (album "b" "Drukqs" (Just 2001)))
          === widths (const "2001   Drukqs")
        widths (\carried -> line carried (song "s" "Vordhosbn" 293 (Just 2)))
          === widths (const "  2   Vordhosbn")
    )
  ,
    ( "the symbol is a different one for each of the three"
    , example do
        symbol Awaited /== symbol Heard
        symbol Heard /== symbol Held
        symbol Held /== symbol Awaited
    )
  ]
 where
  widths :: (Maybe Carried -> Text) -> [Int]
  widths reading = fmap (Width.text . reading) (Nothing : fmap Just [minBound ..])

-- | The mark in the song list, and what its symbol says.
marks :: Checks
marks =
  [
    ( "marking says what playback is doing with the song, in that song's cell"
    , example do
        marked Loading === "  2 " <> symbol Awaited <> " Vordhosbn"
        marked (Sounding Running) === "  2 " <> symbol Heard <> " Vordhosbn"
        marked (Sounding Paused) === "  2 " <> symbol Held <> " Vordhosbn"
    )
  ,
    ( "a loading song carries the symbol a level on its way has its row carry"
    , example (mark Loading === Awaited)
    )
  ,
    ( "marking leaves every other song as its row"
    , example do
        marking (Just (Mark (SongId "t") (Sounding Running))) vordhosbn === row vordhosbn
        marking Nothing vordhosbn === row vordhosbn
    )
  ]
 where
  vordhosbn = song "s" "Vordhosbn" 293 (Just 2)
  marked sound = marking (Just (Mark (SongId "s") sound)) vordhosbn
