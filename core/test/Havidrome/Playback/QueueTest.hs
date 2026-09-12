{- | The album a session plays and the place in it that is playing: what it is
made from, and what it does at either end of the album.
-}
module Havidrome.Playback.QueueTest (tests) where

import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
import Havidrome.Playback.Queue (Queue (playing), backward, forward, songs, startingAt)
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId (..))
import Hedgehog (Group (Group), PropertyT, evalMaybe, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests = Group "Havidrome.Playback.Queue" (making <> goingForward <> goingBack <> wherever)

-- | The album a queue is made from, and the song it starts on.
making :: Checks
making =
  [
    ( "the album it is made from starts at the song it is given"
    , example (fmap (.playing) (startingAt (album 3) (SongId "s2")) === Just (song 2))
    )
  ,
    ( "the album it is made from is nothing when that song is not in the album"
    , example (startingAt (album 3) (SongId "s9") === Nothing)
    )
  ,
    ( "the album it is made from is nothing when the album has no songs"
    , example (startingAt [] (SongId "s1") === Nothing)
    )
  ,
    ( "the album it is made from is kept, in the order it was given"
    , example do
        queue <- album 4 `at` 2
        songs queue === album 4
    )
  ]

-- | Moving on to the next song, and running out at the end.
goingForward :: Checks
goingForward =
  [
    ( "moving forward goes to the next song of the album"
    , example do
        queue <- album 3 `at` 0
        fmap (.playing) (forward queue) === Just (song 2)
    )
  ,
    ( "moving forward runs out after the last song"
    , example do
        queue <- album 3 `at` 2
        forward queue === Nothing
    )
  ,
    ( "moving forward reaches every later song of the album, in order"
    , example do
        queue <- album 4 `at` 1
        fmap (.playing) (walkTo queue) === [song 2, song 3, song 4]
    )
  ]

-- | Moving to the song before, and staying at the start.
goingBack :: Checks
goingBack =
  [
    ( "moving back goes to the previous song of the album"
    , example do
        queue <- album 3 `at` 2
        (backward queue).playing === song 2
    )
  ,
    ( "moving back stays on the first song, which is where going back from it leads"
    , example do
        queue <- album 3 `at` 0
        (backward queue).playing === song 1
    )
  ]

-- | What holds however the queue is moved.
wherever :: Checks
wherever =
  [
    ( "however it is moved, it never leaves the album, and never skips a place in it"
    , property do
        (count, place, steps) <- forAll walking
        queue <- album count `at` place
        (walk steps queue).playing === song (1 + walkedTo count steps place)
    )
  ,
    ( "however it is moved, it keeps the album it was made from"
    , property do
        (count, place, steps) <- forAll walking
        queue <- album count `at` place
        songs (walk steps queue) === album count
    )
  ]
 where
  walking = do
    count <- Gen.int (Range.linear 1 8)
    place <- Gen.int (Range.constant 0 (count - 1))
    steps <- Gen.list (Range.linear 0 30) Gen.bool
    pure (count, place, steps)

-- | An album of so many songs, in album order.
album :: Int -> [Song]
album count = fmap song [1 .. count]

song :: Int -> Song
song n =
  Song
    { id = SongId (T.pack ("s" <> show n))
    , title = T.pack ("Track " <> show n)
    , duration = Seconds 180
    , track = Just n
    , disc = Nothing
    }

{- | The queue over an album that starts at the song in this place, counting
from nothing. An album with no song in that place is the test's own mistake,
and fails it where it is made rather than later.
-}
at :: [Song] -> Int -> PropertyT IO Queue
at album' place = do
  song' <- evalMaybe (listToMaybe (drop place album'))
  evalMaybe (startingAt album' song'.id)

{- | Moving the queue as a run of steps: forward where it can go forward,
backward otherwise.
-}
walk :: [Bool] -> Queue -> Queue
walk steps queue = foldl' move queue steps
 where
  move current forwards
    | forwards = fromMaybe current (forward current)
    | otherwise = backward current

-- | Where those same steps land, counted in places rather than songs.
walkedTo :: Int -> [Bool] -> Int -> Int
walkedTo count steps place = foldl' move place steps
 where
  move current forwards
    | forwards = min (count - 1) (current + 1)
    | otherwise = max 0 (current - 1)

{- | Every song the queue reaches by going forward until the album runs out,
the one it is on first.
-}
walkTo :: Queue -> [Queue]
walkTo queue = queue : foldMap walkTo (forward queue)
