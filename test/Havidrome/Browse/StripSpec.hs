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
import Havidrome.Playback (Playing (Playing))
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

-- | The third song of Drukqs, 4:53 long, this far into it.
vordhosbn :: Int -> Playing
vordhosbn = Playing (drukqsSongs !! 2) . Seconds

-- | The second, 2:09 long, this far into it.
jynweythek :: Int -> Playing
jynweythek = Playing (drukqsSongs !! 1) . Seconds
