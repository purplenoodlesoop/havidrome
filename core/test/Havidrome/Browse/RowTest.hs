{- | What an item reads as on its line: the text an artist, an album or a song
gives its column, the symbol it carries in the cell before its name, and the
turn of the loading symbol's round that symbol is on.
-}
module Havidrome.Browse.RowTest (tests) where

import Data.List (sort)
import Data.Text as T (Text)
import Havidrome.Audio.State (Motion (Paused, Running))
import Havidrome.Browse.Fixtures (album, artist, song)
import Havidrome.Browse.Row
  ( Carried (Awaited, Heard, Held)
  , Mark (Mark)
  , cells
  , line
  , mark
  , marking
  , row
  , symbol
  , turn
  , turns
  )
import Havidrome.Browse.Strip qualified as Strip
import Havidrome.Check (Checks, example)
import Havidrome.Moment (Moment (Moment))
import Havidrome.Playback.Playing (Sound (Loading, Sounding))
import Havidrome.Subsonic.Types (SongId (SongId))
import Havidrome.Width qualified as Width
import Hedgehog (Gen, Group (Group), forAll, property, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests = Group "Havidrome.Browse.Row" (rows <> carrying <> going <> marks)

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
carrying :: Checks
carrying =
  [
    ( "line puts the symbol one space before the name, in the cell row leaves blank"
    , example do
        line (Just Heard) (artist "a" "Aphex Twin") === " " <> symbol Heard <> " Aphex Twin"
        line (Just Heard) (album "b" "Drukqs" (Just 2001))
          === "2001 " <> symbol Heard <> " Drukqs"
        line (Just Heard) (song "s" "Vordhosbn" 293 (Just 2))
          === "  2 " <> symbol Heard <> " Vordhosbn"
    )
  ,
    ( "line keeps every name where row leaves it, whichever symbol the cell carries"
    , example do
        widths (\held -> line held (artist "a" "Aphex Twin")) === widths (const "   Aphex Twin")
        widths (\held -> line held (album "b" "Drukqs" (Just 2001)))
          === widths (const "2001   Drukqs")
        widths (\held -> line held (song "s" "Vordhosbn" 293 (Just 2)))
          === widths (const "  2   Vordhosbn")
    )
  ,
    ( "every symbol a cell can carry takes exactly the one column the cell has"
    , example (fmap (Width.text . symbol) cells === (1 <$ cells))
    )
  ,
    ( "a song whose audio is held carries the very symbol the strip shows for it"
    , example (symbol Held === Strip.held)
    )
  ,
    ( "the symbol for audio running is not the one for audio held"
    , example (symbol Heard /== symbol Held)
    )
  ,
    ( "no turn of the loading symbol reads as either of the other two"
    , example
        (filter (`elem` [symbol Heard, symbol Held]) (fmap (symbol . Awaited) turns) === [])
    )
  ]
 where
  widths :: (Maybe Carried -> Text) -> [Int]
  widths reading = fmap (Width.text . reading) (Nothing : fmap Just cells)

-- | The round the loading symbol's dot goes, and the clock it goes it by.
going :: Checks
going =
  [
    ( "the dot's round is one dot going round a braille cell, ten turns of it"
    , example do
        length turns === 10
        fmap (symbol . Awaited . turn . tenth) [0 .. 9]
          === ["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
    )
  ,
    ( "a tenth of a second later the dot is on the next turn"
    , property do
        at <- forAll tenths
        turn (tenth (at + 1)) /== turn (tenth at)
    )
  ,
    ( "a second holds every turn of the round once, and then it comes round again"
    , property do
        at <- forAll tenths
        sort (fmap (turn . tenth) [at .. at + 9]) === turns
        turn (tenth (at + 10)) === turn (tenth at)
    )
  ]
 where
  tenths :: Gen Int
  tenths = Gen.int (Range.linear 0 999)

{- | The moment this many tenths of a second in, taken half a tenth in so that
a tenth's turn is read and not the rounding at its edge.
-}
tenth :: Int -> Moment
tenth at = Moment (fromIntegral at / 10 + 0.05)

-- | The mark in the song list, and what its symbol says.
marks :: Checks
marks =
  [
    ( "marking says what playback is doing with the song, in that song's cell"
    , example do
        marked Loading === "  2 " <> symbol (Awaited at) <> " Vordhosbn"
        marked (Sounding Running) === "  2 " <> symbol Heard <> " Vordhosbn"
        marked (Sounding Paused) === "  2 " <> symbol Held <> " Vordhosbn"
    )
  ,
    ( "a loading song carries the symbol a level on its way has its row carry"
    , example (mark at Loading === Awaited at)
    )
  ,
    ( "marking leaves every other song as its row"
    , example do
        marking at (Just (Mark (SongId "t") (Sounding Running))) vordhosbn === row vordhosbn
        marking at Nothing vordhosbn === row vordhosbn
    )
  ]
 where
  at = turn (tenth 3)
  vordhosbn = song "s" "Vordhosbn" 293 (Just 2)
  marked sound = marking at (Just (Mark (SongId "s") sound)) vordhosbn
