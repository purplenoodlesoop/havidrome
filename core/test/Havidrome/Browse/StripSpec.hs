-- | The strip along the bottom: the now-playing line it carries, and the
-- reasons that take it over and how long each of them holds it.
--
-- Nothing here plays anything: a strip is told what is playing and what has
-- failed, so the few seconds a skipped track's reason lasts are checked
-- against a clock the spec winds itself, with nothing waited for.
module Havidrome.Browse.StripSpec (spec) where

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
import Havidrome.Playback.Playing
  ( Arrival (Followed, Picked)
  , Playing (Playing)
  , Sound (Loading, Sounding)
  )
import Havidrome.Subsonic.Types (Seconds (Seconds))
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (NonNegative), (===))

spec :: Spec
spec = do
  describe "showing" $ do
    it "has nothing on it before anything has played" $
      showing quiet `shouldBe` Nothing

    it "has nothing on it once the album has run out" $
      showing (played Nothing quiet) `shouldBe` Nothing

    it "carries the overlay for the song playing and how far into it the audio is" $
      showing (playing (vordhosbn 83)) `shouldBe` Just (Overlay (Moment 0) (vordhosbn 83))

    it "carries the song playing now, not the one that was" $
      showing (played (Just (jynweythek 4)) (playing (vordhosbn 83)))
        `shouldBe` Just (Overlay (Moment 0) (jynweythek 4))

    it "carries the moment of the last beat, which turns a loading indicator" $
      showing (beat (Moment 0.35) (Just pickingBtoum) [] quiet)
        `shouldBe` Just (Overlay (Moment 0.35) pickingBtoum)

  describe "overlaid" $ do
    it "lays the track name, the bar and the elapsed and total time across the width" $
      overlaid (Moment 0) 46 (btoum 0) `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

    it "moves the elapsed time on as the audio does" $
      overlaid (Moment 0) 46 (btoum 83) `shouldBe` "⏵ Btoum Roumada  " <> bar 13 3 <> "  1:23 / 1:36"

    it "fills the same part of the bar as has played of the track" $ do
      overlaid (Moment 0) 46 (btoum 24) `shouldBe` "⏵ Btoum Roumada  " <> bar 4 12 <> "  0:24 / 1:36"
      overlaid (Moment 0) 46 (btoum 48) `shouldBe` "⏵ Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
      overlaid (Moment 0) 46 (btoum 96) `shouldBe` "⏵ Btoum Roumada  " <> bar 16 0 <> "  1:36 / 1:36"

    it "fills a column only once a whole column's worth has played" $
      map (filledIn . overlaid (Moment 0) 46 . btoum) [5, 6, 95, 96] `shouldBe` [0, 1, 15, 16]

    it "gives the bar the width the name and the times leave" $ do
      overlaid (Moment 0) 32 (btoum 48) `shouldBe` "⏵ Btoum Roumada  " <> bar 1 1 <> "  0:48 / 1:36"
      overlaid (Moment 0) 82 (btoum 48) `shouldBe` "⏵ Btoum Roumada  " <> bar 26 26 <> "  0:48 / 1:36"

    it "has no bar once the name and the times leave no width for one" $ do
      overlaid (Moment 0) 30 (btoum 48) `shouldBe` "⏵ Btoum Roumada    0:48 / 1:36"
      overlaid (Moment 0) 10 (btoum 48) `shouldBe` "⏵ Btoum Roumada    0:48 / 1:36"

    it "leaves the bar of a track with no length empty, however long it runs" $ do
      overlaid (Moment 0) 32 (silence 0) `shouldBe` "⏵ Silence  " <> bar 0 8 <> "  0:00 / 0:00"
      overlaid (Moment 0) 32 (silence 7) `shouldBe` "⏵ Silence  " <> bar 0 8 <> "  0:07 / 0:00"

    prop "fills exactly the width it is given, wherever in the track the audio is" $
      \(NonNegative spare) (NonNegative seconds) ->
        Text.length (overlaid (Moment 0) (30 + spare) (btoum (seconds `mod` 97))) === 30 + spare

    prop "fills as many whole columns as the part of the track played is worth" $
      \(NonNegative spare) (NonNegative seconds) ->
        let at = seconds `mod` 97
         in filledIn (overlaid (Moment 0) (30 + spare) (btoum at)) === spare * at `div` 96

    prop "never fills less of the bar for more of the track" $
      \width (NonNegative sooner) (NonNegative later) ->
        filledIn (overlaid (Moment 0) width (btoum (min sooner later)))
          <= filledIn (overlaid (Moment 0) width (btoum (max sooner later)))

  describe "whether the song's audio runs or is held" $ do
    it "shows a symbol before the track name while the audio runs" $
      overlaid (Moment 0) 46 (btoum 42) `shouldBe` "⏵ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"

    it "shows a different one in its place while the audio is held" $
      overlaid (Moment 0) 46 (heldBtoum 42) `shouldBe` "⏸ Btoum Roumada  " <> bar 7 9 <> "  0:42 / 1:36"

    it "shows neither while the song loads, however it came to be playing" $ do
      overlaid (Moment 0) 46 pickingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"
      overlaid (Moment 0) 46 followingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

    it "moves nothing else on the line as the symbol comes and goes" $
      map (Text.drop 1 . overlaid (Moment 0) 46) [followingBtoum, btoum 0, heldBtoum 0]
        `shouldBe` replicate 3 (" Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36")

    prop "fills exactly the width it is given, running or held" $
      \(NonNegative spare) (NonNegative seconds) ->
        let at = seconds `mod` 97
         in map (Text.length . overlaid (Moment 0) (30 + spare)) [btoum at, heldBtoum at]
              === replicate 2 (30 + spare)

  describe "a song picked, while it loads" $ do
    it "has a loading indicator where the elapsed time goes, the name, bar and total as usual" $
      overlaid (Moment 0) 46 pickingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠋ / 1:36"

    it "turns the indicator as the beats go by" $ do
      overlaid (Moment 0.35) 46 pickingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠸ / 1:36"
      overlaid (Moment 1.95) 46 pickingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "     ⠏ / 1:36"

    it "gives the elapsed time its place back, the bar unmoved, once the audio has begun" $
      overlaid (Moment 0) 46 (btoum 0) `shouldBe` "⏵ Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

    it "has no indicator for a song the album moved on to, loading or not" $
      overlaid (Moment 0) 46 followingBtoum `shouldBe` "  Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

    prop "fills exactly the width it is given, whatever the moment" $
      \(NonNegative spare) (NonNegative at) ->
        Text.length (overlaid (Moment at) (30 + spare) pickingBtoum) === 30 + spare

    it "gives the whole strip to a failure, the indicator included" $
      showing (beat (Moment 0) (Just pickingBtoum) [broken] quiet) `shouldBe` Just (Wrong brokenly)

  describe "clock" $ do
    it "reads a length in minutes and seconds" $ do
      clock (Seconds 0) `shouldBe` "0:00"
      clock (Seconds 9) `shouldBe` "0:09"
      clock (Seconds 61) `shouldBe` "1:01"
      clock (Seconds 599) `shouldBe` "9:59"

    it "reads in hours as well once there are any" $ do
      clock (Seconds 3600) `shouldBe` "1:00:00"
      clock (Seconds 3725) `shouldBe` "1:02:05"

  describe "a skipped track's reason" $ do
    it "takes the strip over from the overlay" $
      showing (skipped (playing (vordhosbn 83))) `shouldBe` Just (Wrong brokenly)

    it "is still there a second later" $
      showing (beat (Moment 1) (Just (jynweythek 1)) [] (skipped quiet))
        `shouldBe` Just (Wrong brokenly)

    it "gives way to the next song's line a few seconds on" $
      showing (beat (Moment 10) (Just (jynweythek 1)) [] (skipped quiet))
        `shouldBe` Just (Overlay (Moment 10) (jynweythek 1))

    it "keeps its few seconds through a key press" $
      showing (pressed (skipped quiet)) `shouldBe` Just (Wrong brokenly)

  describe "a network failure's reason" $ do
    it "takes the strip over, with nothing playing behind it" $
      showing (unreachable (playing (vordhosbn 83))) `shouldBe` Just (Wrong offlinely)

    it "stays however long the player is left alone" $
      showing (beat (Moment 600) Nothing [] (unreachable quiet))
        `shouldBe` Just (Wrong offlinely)

    it "goes on the next key press" $
      showing (pressed (unreachable quiet)) `shouldBe` Nothing

    it "leaves whatever plays next behind it when a key press takes it down" $
      showing (pressed (beat (Moment 1) (Just (vordhosbn 83)) [] (unreachable quiet)))
        `shouldBe` Just (Overlay (Moment 1) (vordhosbn 83))

  describe "what the player itself has to say" $ do
    it "takes the strip over from the overlay" $
      showing (wrong "The server could not be reached: down" (playing (vordhosbn 0)))
        `shouldBe` Just (Wrong "The server could not be reached: down")

    it "goes on the next key press" $
      showing (pressed (wrong "no answer" quiet)) `shouldBe` Nothing

  describe "the last reason heard in one beat" $
    it "is the one left on the strip" $
      showing (beat (Moment 0) Nothing [broken, offline] quiet) `shouldBe` Just (Wrong offlinely)

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
