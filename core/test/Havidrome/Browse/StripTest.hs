-- | The strip along the bottom: the now-playing line it carries, and the
-- reasons that take it over and how long each of them holds it.
--
-- Nothing here plays anything: a strip is told what is playing and what has
-- failed, so the few seconds a skipped track's reason lasts are checked
-- against a clock the test winds itself, with nothing waited for.
module Havidrome.Browse.StripTest (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Audio.State (Failure (Unplayable, Unreachable), Motion (Paused, Running))
import Havidrome.Browse.Fixtures (bar, drukqsSongs, filledIn, unnumbered)
import Havidrome.Browse.Strip
  ( Moment (Moment)
  , Showing (Overlay, Wrong)
  , Strip
  , beat
  , clock
  , overlaid
  , pressed
  , quiet
  , showing
  , wrong
  )
import Havidrome.Check (example)
import Havidrome.Playback.Playing
  ( Arrival (Followed, Picked)
  , Playing (Playing)
  , Sound (Loading, Sounding)
  )
import Havidrome.Subsonic.Types (Seconds (Seconds))
import Hedgehog (Gen, Group (Group), diff, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Browse.Strip"
    [
      ( "the strip has nothing on it before anything has played"
      , example (showing quiet === Nothing)
      )
    ,
      ( "the strip has nothing on it once the album has run out"
      , example (showing (played Nothing quiet) === Nothing)
      )
    ,
      ( "the strip carries the overlay for the song playing and how far into it the audio is"
      , example (showing (playing (vordhosbn 83)) === Just (Overlay (Moment 0) (vordhosbn 83)))
      )
    ,
      ( "the strip carries the song playing now, not the one that was"
      , example
          ( showing (played (Just (jynweythek 4)) (playing (vordhosbn 83)))
              === Just (Overlay (Moment 0) (jynweythek 4))
          )
      )
    ,
      ( "the strip carries the moment of the last beat, which turns a loading indicator"
      , example
          ( showing (beat (Moment 0.35) (Just pickingBtoum) [] quiet)
              === Just (Overlay (Moment 0.35) pickingBtoum)
          )
      )
    ,
      ( "the overlay lays the track name, the bar and the elapsed and total time across the width"
      , example (overlaid (Moment 0) 46 (btoum 0) === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")
      )
    ,
      ( "the overlay moves the elapsed time on as the audio does"
      , example (overlaid (Moment 0) 46 (btoum 83) === "⏵ Btoum Roumada  " <> bar 13 3 <> "  1:23 / 1:36")
      )
    ,
      ( "the overlay fills the same part of the bar as has played of the track"
      , example do
          overlaid (Moment 0) 46 (btoum 24) === "⏵ Btoum Roumada  " <> bar 4 12 <> "  0:24 / 1:36"
          overlaid (Moment 0) 46 (btoum 48) === "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
          overlaid (Moment 0) 46 (btoum 96) === "⏵ Btoum Roumada  " <> bar 16 0 <> "  1:36 / 1:36"
      )
    ,
      ( "the overlay fills a column only once a whole column's worth has played"
      , example
          (map (filledIn . overlaid (Moment 0) 46 . btoum) [5, 6, 95, 96] === [0, 1, 15, 16])
      )
    ,
      ( "the overlay gives the bar the width the name and the times leave"
      , example do
          overlaid (Moment 0) 32 (btoum 48) === "⏵ Btoum Roumada  " <> bar 1 1 <> "  0:48 / 1:36"
          overlaid (Moment 0) 82 (btoum 48) === "⏵ Btoum Roumada  " <> bar 26 26 <> "  0:48 / 1:36"
      )
    ,
      ( "the overlay has no bar once the name and the times leave no width for one"
      , example do
          overlaid (Moment 0) 30 (btoum 48) === "⏵ Btoum Roumada    0:48 / 1:36"
          overlaid (Moment 0) 10 (btoum 48) === "⏵ Btoum Roumada    0:48 / 1:36"
      )
    ,
      ( "the overlay leaves the bar of a track with no length empty, however long it runs"
      , example do
          overlaid (Moment 0) 32 (silence 0) === "⏵ Silence  " <> bar 0 8 <> "  0:00 / 0:00"
          overlaid (Moment 0) 32 (silence 7) === "⏵ Silence  " <> bar 0 8 <> "  0:07 / 0:00"
      )
    ,
      ( "the overlay fills exactly the width it is given, wherever in the track the audio is"
      , property do
          spare <- forAll spareWidth
          at <- forAll intoBtoum
          Text.length (overlaid (Moment 0) (30 + spare) (btoum at)) === 30 + spare
      )
    ,
      ( "the overlay fills as many whole columns as the part of the track played is worth"
      , property do
          spare <- forAll spareWidth
          at <- forAll intoBtoum
          filledIn (overlaid (Moment 0) (30 + spare) (btoum at)) === spare * at `div` 96
      )
    ,
      ( "the overlay never fills less of the bar for more of the track"
      , property do
          width <- forAll (Gen.int (Range.linearFrom 0 (-20) 120))
          sooner <- forAll (Gen.int (Range.linear 0 200))
          later <- forAll (Gen.int (Range.linear 0 200))
          diff
            (filledIn (overlaid (Moment 0) width (btoum (min sooner later))))
            (<=)
            (filledIn (overlaid (Moment 0) width (btoum (max sooner later))))
      )
    ,
      ( "the overlay shows a symbol before the track name while the audio runs"
      , example (overlaid (Moment 0) 46 (btoum 42) === "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36")
      )
    ,
      ( "the overlay shows a different one in its place while the audio is held"
      , example (overlaid (Moment 0) 46 (heldBtoum 42) === "⏸ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36")
      )
    ,
      ( "the overlay shows neither while the song loads, however it came to be playing"
      , example do
          overlaid (Moment 0) 46 pickingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"
          overlaid (Moment 0) 46 followingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"
      )
    ,
      ( "the overlay moves nothing else on the line as the symbol comes and goes"
      , example
          ( map (Text.drop 1 . overlaid (Moment 0) 46) [followingBtoum, btoum 0, heldBtoum 0]
              === replicate 3 (" Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")
          )
      )
    ,
      ( "the overlay fills exactly the width it is given, running or held"
      , property do
          spare <- forAll spareWidth
          at <- forAll intoBtoum
          map (Text.length . overlaid (Moment 0) (30 + spare)) [btoum at, heldBtoum at]
            === replicate 2 (30 + spare)
      )
    ,
      ( "a song picked has a loading indicator where the elapsed time goes, the name, bar and total as usual"
      , example (overlaid (Moment 0) 46 pickingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36")
      )
    ,
      ( "a song picked turns the indicator as the beats go by"
      , example do
          overlaid (Moment 0.35) 46 pickingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "     ⠸ / 1:36"
          overlaid (Moment 1.95) 46 pickingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "     ⠏ / 1:36"
      )
    ,
      ( "a song picked gives the elapsed time its place back, the bar unmoved, once the audio has begun"
      , example (overlaid (Moment 0) 46 (btoum 0) === "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")
      )
    ,
      ( "a song the album moved on to has no indicator, loading or not"
      , example (overlaid (Moment 0) 46 followingBtoum === "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")
      )
    ,
      ( "a song picked fills exactly the width it is given, whatever the moment"
      , property do
          spare <- forAll spareWidth
          at <- forAll (Gen.double (Range.linearFrac 0 120))
          Text.length (overlaid (Moment at) (30 + spare) pickingBtoum) === 30 + spare
      )
    ,
      ( "a failure takes the whole strip, the indicator included"
      , example
          (showing (beat (Moment 0) (Just pickingBtoum) [broken] quiet) === Just (Wrong brokenly))
      )
    ,
      ( "clock reads a length in minutes and seconds"
      , example do
          clock (Seconds 0) === "0:00"
          clock (Seconds 9) === "0:09"
          clock (Seconds 61) === "1:01"
          clock (Seconds 599) === "9:59"
      )
    ,
      ( "clock reads in hours as well once there are any"
      , example do
          clock (Seconds 3600) === "1:00:00"
          clock (Seconds 3725) === "1:02:05"
      )
    ,
      ( "a skipped track's reason takes the strip over from the overlay"
      , example (showing (skipped (playing (vordhosbn 83))) === Just (Wrong brokenly))
      )
    ,
      ( "a skipped track's reason is still there a second later"
      , example
          ( showing (beat (Moment 1) (Just (jynweythek 1)) [] (skipped quiet))
              === Just (Wrong brokenly)
          )
      )
    ,
      ( "a skipped track's reason gives way to the next song's line a few seconds on"
      , example
          ( showing (beat (Moment 10) (Just (jynweythek 1)) [] (skipped quiet))
              === Just (Overlay (Moment 10) (jynweythek 1))
          )
      )
    ,
      ( "a skipped track's reason keeps its few seconds through a key press"
      , example (showing (pressed (skipped quiet)) === Just (Wrong brokenly))
      )
    ,
      ( "a network failure's reason takes the strip over, with nothing playing behind it"
      , example (showing (unreachable (playing (vordhosbn 83))) === Just (Wrong offlinely))
      )
    ,
      ( "a network failure's reason stays however long the player is left alone"
      , example
          ( showing (beat (Moment 600) Nothing [] (unreachable quiet))
              === Just (Wrong offlinely)
          )
      )
    ,
      ( "a network failure's reason goes on the next key press"
      , example (showing (pressed (unreachable quiet)) === Nothing)
      )
    ,
      ( "a network failure's reason leaves whatever plays next behind it when a key press takes it down"
      , example
          ( showing (pressed (beat (Moment 1) (Just (vordhosbn 83)) [] (unreachable quiet)))
              === Just (Overlay (Moment 1) (vordhosbn 83))
          )
      )
    ,
      ( "what the player itself has to say takes the strip over from the overlay"
      , example
          ( showing (wrong "The server could not be reached: down" (playing (vordhosbn 0)))
              === Just (Wrong "The server could not be reached: down")
          )
      )
    ,
      ( "what the player itself has to say goes on the next key press"
      , example (showing (pressed (wrong "no answer" quiet)) === Nothing)
      )
    ,
      ( "the last reason heard in one beat is the one left on the strip"
      , example
          (showing (beat (Moment 0) Nothing [broken, offline] quiet) === Just (Wrong offlinely))
      )
    ]

-- | Whatever the name and the times leave of a width, which is the bar's.
spareWidth :: Gen Int
spareWidth = Gen.int (Range.linear 0 60)

-- | A position inside the song the overlay tests are laid out against.
intoBtoum :: Gen Int
intoBtoum = Gen.int (Range.linear 0 96)

-- | The strip a beat leaves behind at the start of the clock, with nothing
-- having failed since the last one.
played :: Maybe Playing -> Strip -> Strip
played playing' = beat (Moment 0) playing' []

-- | A strip with this song playing and nothing wrong.
playing :: Playing -> Strip
playing = flip played quiet . Just

-- | The strip a beat leaves behind when the track it has just moved on from
-- would not play, at the start of the clock.
skipped :: Strip -> Strip
skipped = beat (Moment 0) (Just (jynweythek 0)) [broken]

-- | The same for a server that could not be reached, after which nothing is
-- playing at all.
unreachable :: Strip -> Strip
unreachable = beat (Moment 0) Nothing [offline]

broken, offline :: Failure
broken = Unplayable "the file will not play: it is broken"
offline = Unreachable "the server could not be reached: it is down"

-- | How each of those reads on the strip: the backend's reason as a sentence
-- of its own.
brokenly, offlinely :: Text
brokenly = "The file will not play: it is broken"
offlinely = "The server could not be reached: it is down"

-- | The first song of Drukqs, 1:36 long, picked and its audio running, this
-- far into it. Its symbol, name and times take 30 columns with the gaps
-- between them, so each column past those is one of the bar's.
btoum :: Int -> Playing
btoum at = Playing (drukqsSongs !! 0) (Seconds at) Picked (Sounding Running)

-- | The same song, its audio started and held this far into it.
heldBtoum :: Int -> Playing
heldBtoum at = Playing (drukqsSongs !! 0) (Seconds at) Picked (Sounding Paused)

-- | The same song just picked, its audio not yet started.
pickingBtoum :: Playing
pickingBtoum = Playing (drukqsSongs !! 0) (Seconds 0) Picked Loading

-- | The same song moved on to by the album, its audio not yet started.
followingBtoum :: Playing
followingBtoum = Playing (drukqsSongs !! 0) (Seconds 0) Followed Loading

-- | The third song of Drukqs, 4:53 long, picked and its audio running, this
-- far into it.
vordhosbn :: Int -> Playing
vordhosbn at = Playing (drukqsSongs !! 2) (Seconds at) Picked (Sounding Running)

-- | The second, 2:09 long, picked and its audio running, this far into it.
jynweythek :: Int -> Playing
jynweythek at = Playing (drukqsSongs !! 1) (Seconds at) Picked (Sounding Running)

-- | A song the server gives no length for, picked and its audio running, this
-- far into it.
silence :: Int -> Playing
silence at = Playing (unnumbered "s9" "Silence" 0) (Seconds at) Picked (Sounding Running)
