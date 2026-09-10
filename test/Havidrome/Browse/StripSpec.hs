{-# LANGUAGE OverloadedStrings #-}

-- | The strip along the bottom: the now-playing line it carries, and the
-- reasons that take it over and how long each of them holds it.
--
-- Nothing here plays anything: a strip is told what is playing and what has
-- failed, so the few seconds a skipped track's reason lasts are checked
-- against a clock the spec winds itself, with nothing waited for.
module Havidrome.Browse.StripSpec (spec) where

import Data.Text (Text)
import Havidrome.Audio (Failure (Unplayable, Unreachable))
import Havidrome.Browse.Fixtures (drukqsSongs)
import Havidrome.Browse.Strip
  ( Moment (Moment)
  , Showing (Overlay, Wrong)
  , Strip
  , beat
  , clock
  , pressed
  , quiet
  , showing
  , wrong
  )
import Havidrome.Playback (Arrival (Followed, Picked), Playing (Playing))
import Havidrome.Subsonic (Seconds (Seconds))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "showing" $ do
    it "has nothing on it before anything has played" $
      showing quiet `shouldBe` Nothing

    it "has nothing on it once the album has run out" $
      showing (played Nothing quiet) `shouldBe` Nothing

    it "carries the track name and its elapsed and total time" $
      showing (playing (vordhosbn 0)) `shouldBe` Just (Overlay "Vordhosbn  0:00 / 4:53")

    it "moves the elapsed time on as the audio does" $
      showing (playing (vordhosbn 83)) `shouldBe` Just (Overlay "Vordhosbn  1:23 / 4:53")

    it "carries the song playing now, not the one that was" $
      showing (played (Just (jynweythek 4)) (playing (vordhosbn 83)))
        `shouldBe` Just (Overlay "Jynweythek  0:04 / 2:09")

  describe "a song picked, while it loads" $ do
    it "carries a loading indicator in place of the elapsed time, beside the track name" $
      showing (playing pickingVordhosbn) `shouldBe` Just (Overlay "Vordhosbn  ⠋ / 4:53")

    it "turns the indicator as the beats go by" $ do
      showing (beat (Moment 0.35) (Just pickingVordhosbn) [] quiet)
        `shouldBe` Just (Overlay "Vordhosbn  ⠸ / 4:53")
      showing (beat (Moment 1.95) (Just pickingVordhosbn) [] quiet)
        `shouldBe` Just (Overlay "Vordhosbn  ⠏ / 4:53")

    it "gives the elapsed time back its place once the audio has begun" $
      showing (played (Just (vordhosbn 0)) (playing pickingVordhosbn))
        `shouldBe` Just (Overlay "Vordhosbn  0:00 / 4:53")

    it "carries no indicator for a song the album moved on to, loading or not" $
      showing (playing followingJynweythek) `shouldBe` Just (Overlay "Jynweythek  0:00 / 2:09")

    it "gives the whole strip to a failure, the indicator included" $
      showing (beat (Moment 0) (Just pickingVordhosbn) [broken] quiet) `shouldBe` Just (Wrong brokenly)

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
        `shouldBe` Just (Overlay "Jynweythek  0:01 / 2:09")

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
        `shouldBe` Just (Overlay "Vordhosbn  1:23 / 4:53")

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

-- | The third song of Drukqs, 4:53 long, picked and its audio started, this
-- far into it.
vordhosbn :: Int -> Playing
vordhosbn at = Playing (drukqsSongs !! 2) (Seconds at) Picked True

-- | The second, 2:09 long, picked and its audio started, this far into it.
jynweythek :: Int -> Playing
jynweythek at = Playing (drukqsSongs !! 1) (Seconds at) Picked True

-- | The third song picked, its audio not yet started.
pickingVordhosbn :: Playing
pickingVordhosbn = Playing (drukqsSongs !! 2) (Seconds 0) Picked False

-- | The second song, moved on to by the album, its audio not yet started.
followingJynweythek :: Playing
followingJynweythek = Playing (drukqsSongs !! 1) (Seconds 0) Followed False
