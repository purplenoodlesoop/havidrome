module Havidrome.Playback.QueueSpec (spec) where

import Data.Maybe (fromMaybe, isNothing)
import Data.Text qualified as Text
import Havidrome.Playback.Queue
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId (..))
import Test.Hspec
import Test.QuickCheck

-- | An album of so many songs, in album order.
album :: Int -> [Song]
album count = map song [1 .. count]

song :: Int -> Song
song n =
  Song
    { id = SongId (Text.pack ("s" <> show n))
    , title = Text.pack ("Track " <> show n)
    , duration = Seconds 180
    , track = Just n
    , disc = Nothing
    }

-- | The queue over an album that starts at the song in this place, counting
-- from nothing.
at :: [Song] -> Int -> Queue
at songs' place =
  fromMaybe (error "the album has no song in that place") $
    startingAt songs' (songs' !! place).id

-- | Moving the queue as a run of steps: forward where it can go forward,
-- backward otherwise.
walk :: [Bool] -> Queue -> Queue
walk steps queue = foldl move queue steps
 where
  move current forwards
    | forwards = fromMaybe current (forward current)
    | otherwise = backward current

-- | Where those same steps land, counted in places rather than songs.
walkedTo :: Int -> [Bool] -> Int -> Int
walkedTo count steps place = foldl move place steps
 where
  move current forwards
    | forwards = min (count - 1) (current + 1)
    | otherwise = max 0 (current - 1)

spec :: Spec
spec = do
  describe "the album it is made from" $ do
    it "starts at the song it is given" $
      fmap (.playing) (startingAt (album 3) (SongId "s2")) `shouldBe` Just (song 2)

    it "is nothing when that song is not in the album" $
      startingAt (album 3) (SongId "s9") `shouldSatisfy` isNothing

    it "is nothing when the album has no songs" $
      startingAt [] (SongId "s1") `shouldSatisfy` isNothing

    it "keeps the album, in the order it was given" $
      songs (album 4 `at` 2) `shouldBe` album 4

  describe "moving forward" $ do
    it "goes to the next song of the album" $
      fmap (.playing) (forward (album 3 `at` 0)) `shouldBe` Just (song 2)

    it "runs out after the last song" $
      forward (album 3 `at` 2) `shouldSatisfy` isNothing

    it "reaches every later song of the album, in order" $
      map (.playing) (walkTo (album 4 `at` 1)) `shouldBe` [song 2, song 3, song 4]

  describe "moving back" $ do
    it "goes to the previous song of the album" $
      (backward (album 3 `at` 2)).playing `shouldBe` song 2

    it "stays on the first song, which is where going back from it leads" $
      (backward (album 3 `at` 0)).playing `shouldBe` song 1

  describe "however it is moved" $ do
    it "never leaves the album, and never skips a place in it" $
      property $ \(Positive count) (NonNegative offset) steps ->
        let place = offset `mod` count
            walked = walk steps (album count `at` place)
         in walked.playing == song (1 + walkedTo count steps place)

    it "keeps the album it was made from" $
      property $ \(Positive count) (NonNegative offset) steps ->
        let place = offset `mod` count
         in songs (walk steps (album count `at` place)) == album count

-- | Every song the queue reaches by going forward until the album runs out,
-- the one it is on first.
walkTo :: Queue -> [Queue]
walkTo queue = queue : maybe [] walkTo (forward queue)
