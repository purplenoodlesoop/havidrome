{-# LANGUAGE OverloadedStrings #-}

-- | The strip along the bottom: the now-playing line it carries, and the
-- reasons that take it over and how long each of them holds it.
--
-- Nothing here plays anything: a strip is told what is playing and what has
-- failed, so the few seconds a skipped track's reason lasts are checked
-- against a clock the spec winds itself, with nothing waited for.
module Havidrome.Browse.StripSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Audio (Failure (Unplayable, Unreachable))
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
import Havidrome.Playback (Playing (Playing))
import Havidrome.Subsonic (Seconds (Seconds))
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
      showing (playing (vordhosbn 83)) `shouldBe` Just (Overlay (vordhosbn 83))

    it "carries the song playing now, not the one that was" $
      showing (played (Just (jynweythek 4)) (playing (vordhosbn 83)))
        `shouldBe` Just (Overlay (jynweythek 4))

  describe "overlaid" $ do
    it "lays the track name, the bar and the elapsed and total time across the width" $
      overlaid 44 (btoum 0) `shouldBe` "Btoum Roumada  " <> bar 0 16 <> "  0:00 / 1:36"

    it "moves the elapsed time on as the audio does" $
      overlaid 44 (btoum 83) `shouldBe` "Btoum Roumada  " <> bar 13 3 <> "  1:23 / 1:36"

    it "fills the same part of the bar as has played of the track" $ do
      overlaid 44 (btoum 24) `shouldBe` "Btoum Roumada  " <> bar 4 12 <> "  0:24 / 1:36"
      overlaid 44 (btoum 48) `shouldBe` "Btoum Roumada  " <> bar 8 8 <> "  0:48 / 1:36"
      overlaid 44 (btoum 96) `shouldBe` "Btoum Roumada  " <> bar 16 0 <> "  1:36 / 1:36"

    it "fills a column only once a whole column's worth has played" $
      map (filledIn . overlaid 44 . btoum) [5, 6, 95, 96] `shouldBe` [0, 1, 15, 16]

    it "gives the bar the width the name and the times leave" $ do
      overlaid 30 (btoum 48) `shouldBe` "Btoum Roumada  " <> bar 1 1 <> "  0:48 / 1:36"
      overlaid 80 (btoum 48) `shouldBe` "Btoum Roumada  " <> bar 26 26 <> "  0:48 / 1:36"

    it "has no bar once the name and the times leave no width for one" $ do
      overlaid 28 (btoum 48) `shouldBe` "Btoum Roumada    0:48 / 1:36"
      overlaid 10 (btoum 48) `shouldBe` "Btoum Roumada    0:48 / 1:36"

    it "leaves the bar of a track with no length empty, however long it runs" $ do
      overlaid 30 (silence 0) `shouldBe` "Silence  " <> bar 0 8 <> "  0:00 / 0:00"
      overlaid 30 (silence 7) `shouldBe` "Silence  " <> bar 0 8 <> "  0:07 / 0:00"

    prop "fills exactly the width it is given, wherever in the track the audio is" $
      \(NonNegative spare) (NonNegative seconds) ->
        Text.length (overlaid (28 + spare) (btoum (seconds `mod` 97))) === 28 + spare

    prop "fills as many whole columns as the part of the track played is worth" $
      \(NonNegative spare) (NonNegative seconds) ->
        let at = seconds `mod` 97
         in filledIn (overlaid (28 + spare) (btoum at)) === spare * at `div` 96

    prop "never fills less of the bar for more of the track" $
      \width (NonNegative sooner) (NonNegative later) ->
        filledIn (overlaid width (btoum (min sooner later)))
          <= filledIn (overlaid width (btoum (max sooner later)))

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
        `shouldBe` Just (Overlay (jynweythek 1))

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
        `shouldBe` Just (Overlay (vordhosbn 83))

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

-- | The first song of Drukqs, 1:36 long, this far into it. Its name and times
-- take 28 columns with the gaps between them, so each column past those is
-- one of the bar's.
btoum :: Int -> Playing
btoum = Playing (drukqsSongs !! 0) . Seconds

-- | The third song of Drukqs, 4:53 long, this far into it.
vordhosbn :: Int -> Playing
vordhosbn = Playing (drukqsSongs !! 2) . Seconds

-- | The second, 2:09 long, this far into it.
jynweythek :: Int -> Playing
jynweythek = Playing (drukqsSongs !! 1) . Seconds

-- | A song the server gives no length for, this far into it.
silence :: Int -> Playing
silence = Playing (unnumbered "s9" "Silence" 0) . Seconds
