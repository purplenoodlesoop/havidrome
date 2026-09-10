-- | The album a session plays, and the place in it that is playing.
--
-- A queue is made from an album's songs and one of them, and it can only ever
-- be moved one place at a time: there is no way to reorder it, to put a song
-- into it, or to land on a song that is not in it. Album order and the
-- boundary of the album are therefore properties of this type, not rules the
-- session has to remember.
--
-- Nothing here performs I\/O, so all of it is exercised on its own.
module Havidrome.Playback.Queue
  ( -- * The album, and the place in it
    Queue
  , startingAt
  , playing
  , songs

    -- * Moving through it
  , forward
  , backward
  ) where

import Havidrome.Subsonic.Types (Song (..), SongId)

-- | An album's songs with one of them playing: those before it, nearest
-- first, and those after it, in album order.
data Queue = Queue
  { queueBefore :: [Song]
  , queuePlaying :: Song
  , queueAfter :: [Song]
  }
  deriving stock (Eq, Show)

-- | The queue over an album that starts at one of its songs, or nothing when
-- that song is not one of the album's. The songs are taken in the order they
-- are given, which is the album order the client lists them in.
startingAt :: [Song] -> SongId -> Maybe Queue
startingAt album wanted = go [] album
 where
  go _ [] = Nothing
  go before (song : after)
    | songId song == wanted = Just (Queue before song after)
    | otherwise = go (song : before) after

-- | The song the queue is on.
playing :: Queue -> Song
playing = queuePlaying

-- | The whole album, in album order. It never changes as the queue moves.
songs :: Queue -> [Song]
songs queue = reverse (queueBefore queue) <> (queuePlaying queue : queueAfter queue)

-- | The next song of the album, or nothing on the last song — where the
-- album, and with it the playing, runs out.
forward :: Queue -> Maybe Queue
forward queue = case queueAfter queue of
  [] -> Nothing
  song : rest ->
    Just
      Queue
        { queueBefore = queuePlaying queue : queueBefore queue
        , queuePlaying = song
        , queueAfter = rest
        }

-- | The previous song of the album; on the first song, that same song, so
-- that going back there is going back to its beginning.
backward :: Queue -> Queue
backward queue = case queueBefore queue of
  [] -> queue
  song : rest ->
    Queue
      { queueBefore = rest
      , queuePlaying = song
      , queueAfter = queuePlaying queue : queueAfter queue
      }
