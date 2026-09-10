{-# LANGUAGE OverloadedStrings #-}

-- | The whole of a playback session, driven against a stand-in backend: no
-- audio device, no server, and nothing of the terminal.
module Havidrome.PlaybackSpec (spec) where

import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Audio.State (Failure (..), Motion (..))
import Havidrome.Playback
import Havidrome.Playback.Standin
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId (..))
import Test.Hspec

-- | An album of so many songs, in album order, told apart from another
-- album's by the name its songs are lettered with.
album :: Text -> Int -> [Song]
album name count = map song [1 .. count]
 where
  song n =
    Song
      { songId = SongId (name <> Text.pack (show n))
      , songTitle = name <> Text.pack (" track " <> show n)
      , songDuration = Seconds 180
      , songTrack = Just n
      , songDisc = Nothing
      }

-- | The queue over an album that starts at the song in this place, counting
-- from nothing.
at :: [Song] -> Int -> Queue
at tracks place =
  fromMaybe (error "the album has no song in that place") $
    startingAt tracks (songId (tracks !! place))

-- | What the backend is told when that song is played from its beginning.
from :: Song -> (Text, Seconds)
from song = (address (songId song), Seconds 0)

-- | Nothing is asked of the session: the audio simply runs out, so many
-- times, and the session is handed what the backend says about it. This is
-- the whole of \"without further input\".
runOut :: Standin -> Session -> Int -> IO [Failure]
runOut standin session times =
  fmap concat . replicateM times $ do
    finish standin
    attend session

-- | The song a session is playing, if it is playing one.
current :: Session -> IO (Maybe Song)
current = fmap (fmap playingSong) . nowPlaying

spec :: Spec
spec = do
  let tracks = album "a" 4

  describe "picking a song" $ do
    it "plays that song, from its beginning" $ withStandin $ \standin session -> do
      start session (tracks `at` 1)
      loaded standin `shouldReturn` [from (tracks !! 1)]

    it "then plays the rest of its album, in album order, with nothing more asked" $
      withStandin $ \standin session -> do
        start session (tracks `at` 1)
        shown <- runOut standin session 3
        shown `shouldBe` []
        loaded standin `shouldReturn` map from (drop 1 tracks)

    it "plays nothing outside that album, however long it is left alone" $
      withStandin $ \standin session -> do
        start session (tracks `at` 0)
        _ <- runOut standin session 8
        loaded standin `shouldReturn` map from tracks

    it "leaves nothing playing once the album has run out" $
      withStandin $ \standin session -> do
        start session (tracks `at` 2)
        _ <- runOut standin session 2
        nowPlaying session `shouldReturn` Nothing
        motionOf standin `shouldReturn` Nothing

  describe "picking a song of another album" $
    it "replaces what is playing, and then goes on through that album" $
      withStandin $ \standin session -> do
        let other = album "b" 3
        start session (tracks `at` 0)
        start session (other `at` 1)
        _ <- runOut standin session 2
        current session `shouldReturn` Nothing
        loaded standin `shouldReturn` (from (tracks !! 0) : map from (drop 1 other))

  describe "next" $ do
    it "plays the next song of the album" $ withStandin $ \standin session -> do
      start session (tracks `at` 0)
      next session
      current session `shouldReturn` Just (tracks !! 1)
      loaded standin `shouldReturn` map from (take 2 tracks)

    it "ends the playing on the last song" $ withStandin $ \standin session -> do
      start session (tracks `at` 3)
      next session
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

    it "starts nothing more once it has ended the playing" $
      withStandin $ \standin session -> do
        start session (tracks `at` 3)
        next session
        next session
        _ <- runOut standin session 3
        loaded standin `shouldReturn` [from (tracks !! 3)]

  describe "previous" $ do
    it "goes to the previous song even part-way through this one" $
      withStandin $ \standin session -> do
        start session (tracks `at` 2)
        reach standin (Seconds 90)
        previous session
        current session `shouldReturn` Just (tracks !! 1)
        loaded standin `shouldReturn` [from (tracks !! 2), from (tracks !! 1)]

    it "plays the first song again when it is on the first" $
      withStandin $ \standin session -> do
        start session (tracks `at` 0)
        reach standin (Seconds 90)
        previous session
        current session `shouldReturn` Just (tracks !! 0)
        loaded standin `shouldReturn` [from (tracks !! 0), from (tracks !! 0)]

  describe "a song that will not play" $ do
    it "is skipped: its failure is shown and the next song of the album starts" $
      withStandin $ \standin session -> do
        let failure = Unplayable "the file will not play: unrecognized file format"
        start session (tracks `at` 1)
        breakWith standin failure
        attend session `shouldReturn` [failure]
        current session `shouldReturn` Just (tracks !! 2)
        loaded standin `shouldReturn` [from (tracks !! 1), from (tracks !! 2)]

    it "ends the playing when it was the album's last song" $
      withStandin $ \standin session -> do
        let failure = Unplayable "the file will not play: unrecognized file format"
        start session (tracks `at` 3)
        breakWith standin failure
        attend session `shouldReturn` [failure]
        nowPlaying session `shouldReturn` Nothing

  describe "a server that cannot be reached" $ do
    it "stops the playing and shows the failure" $ withStandin $ \standin session -> do
      let failure = Unreachable "the server could not be reached: loading failed"
      start session (tracks `at` 1)
      breakWith standin failure
      attend session `shouldReturn` [failure]
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

    it "starts no further song of the album" $ withStandin $ \standin session -> do
      start session (tracks `at` 1)
      breakWith standin (Unreachable "the server could not be reached: loading failed")
      _ <- attend session
      _ <- runOut standin session 3
      loaded standin `shouldReturn` [from (tracks !! 1)]

  describe "pausing, resuming and seeking" $ do
    it "holds the audio, and leaves the song where it is" $
      withStandin $ \standin session -> do
        start session (tracks `at` 1)
        reach standin (Seconds 60)
        pause session
        motionOf standin `shouldReturn` Just Paused
        nowPlaying session `shouldReturn` Just (Playing (tracks !! 1) (Seconds 60))
        loaded standin `shouldReturn` [from (tracks !! 1)]

    it "lets it run on from where it was held" $ withStandin $ \standin session -> do
      start session (tracks `at` 1)
      reach standin (Seconds 60)
      pause session
      resume session
      motionOf standin `shouldReturn` Just Running
      nowPlaying session `shouldReturn` Just (Playing (tracks !! 1) (Seconds 60))

    it "moves through the song without changing which song it is" $
      withStandin $ \standin session -> do
        start session (tracks `at` 1)
        reach standin (Seconds 60)
        seekBy session 30
        nowPlaying session `shouldReturn` Just (Playing (tracks !! 1) (Seconds 90))
        loaded standin `shouldReturn` [from (tracks !! 1)]

    it "leaves the rest of the album to play as it would have" $
      withStandin $ \standin session -> do
        start session (tracks `at` 1)
        pause session
        resume session
        seekBy session 30
        _ <- runOut standin session 3
        loaded standin `shouldReturn` map from (drop 1 tracks)

  describe "stopping" $
    it "plays nothing, and leaves no album behind to carry on" $
      withStandin $ \standin session -> do
        start session (tracks `at` 1)
        stop session
        nowPlaying session `shouldReturn` Nothing
        motionOf standin `shouldReturn` Nothing
        _ <- runOut standin session 3
        loaded standin `shouldReturn` [from (tracks !! 1)]
