{- | The album a session plays, and the place in it that is playing.

A queue is made from an album's songs and one of them, and it can only ever
be moved one place at a time: there is no way to reorder it, to put a song
into it, or to land on a song that is not in it. Album order and the
boundary of the album are therefore properties of this type, not rules the
session has to remember.

Nothing here performs I\/O, so all of it is exercised on its own.
-}
module Havidrome.Playback.Queue
  ( -- * The album, and the place in it
    Queue (playing)
  , startingAt
  , songs

    -- * Moving through it
  , forward
  , backward
  ) where

import GHC.Generics (Generic)
import Havidrome.Subsonic.Types (Song (..), SongId)

{- | An album's songs with one of them playing: those before it, nearest
first, and those after it, in album order.
-}
data Queue = Queue
  { before :: [Song]
  , playing :: Song
  , after :: [Song]
  }
  deriving stock (Eq, Generic, Show)

{- | The queue over an album that starts at one of its songs, or nothing when
that song is not one of the album's. The songs are taken in the order they
are given, which is the album order the client lists them in.
-}
startingAt :: [Song] -> SongId -> Maybe Queue
startingAt album wanted = go [] album
 where
  go _ [] = Nothing
  go before (song : after)
    | song.id == wanted = Just (Queue before song after)
    | otherwise = go (song : before) after

-- | The whole album, in album order. It never changes as the queue moves.
songs :: Queue -> [Song]
songs queue = reverse queue.before <> (queue.playing : queue.after)

{- | The next song of the album, or nothing on the last song — where the
album, and with it the playing, runs out.
-}
forward :: Queue -> Maybe Queue
forward queue = case queue.after of
  [] -> Nothing
  song : rest ->
    Just
      Queue
        { before = queue.playing : queue.before
        , playing = song
        , after = rest
        }

{- | The previous song of the album; on the first song, that same song, so
that going back there is going back to its beginning.
-}
backward :: Queue -> Queue
backward queue = case queue.before of
  [] -> queue
  song : rest ->
    Queue
      { before = rest
      , playing = song
      , after = queue.playing : queue.after
      }
