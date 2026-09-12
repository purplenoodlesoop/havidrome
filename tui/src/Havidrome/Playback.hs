-- | The playing of an album: what is playing now, what plays next, and what
-- the four controls over it do. It sits between the lists the user browses
-- and the backend that makes the sound, and it is the only thing that decides
-- which song plays.
--
-- A session is created by picking a song: playback starts there and runs
-- through the rest of that song's album, in album order, without anything
-- more being asked of the user. The album it is in is a 'Queue', which is
-- what keeps the order and the album's edges; this module only moves through
-- one and tells the backend what to play.
--
-- A session is a record of operations, each closed over the place in the
-- album it works on, so the place itself belongs to the session and leaves no
-- mark on any type. Nothing here touches a terminal or a server, and the
-- backend it drives is a record of operations too, so a session runs the
-- whole album against a stand-in that makes no sound.
module Havidrome.Playback
  ( -- * A session
    Session (..)
  , newSession

    -- * What it is playing
  , module Havidrome.Playback.Playing

    -- * The album being played
  , module Havidrome.Playback.Queue
  ) where

import Control.Concurrent.MVar (modifyMVar, modifyMVar_, newMVar, readMVar)
import Data.Text (Text)
import GHC.Generics (Generic)
import Havidrome.Audio
  ( Audio (..)
  , Event (..)
  , Failure (..)
  , Motion (..)
  , Playback (..)
  , State (..)
  , Track (..)
  )
import Havidrome.Playback.Playing
import Havidrome.Playback.Queue
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId)
import Optics.Core (view, (%))

-- | An album played through one audio backend: everything the player can ask
-- of it, and everything it has to say back. How far into a song the audio has
-- come is the backend's to say, and is asked of it rather than kept here
-- twice.
data Session = Session
  { -- | Play this album from the song it is on, in place of whatever was
    -- playing.
    start :: Queue -> IO ()
  , -- | Play the next song of the album; on the last song, end the playing.
    next :: IO ()
  , -- | Play the previous song of the album, from its beginning, however far
    -- into the current song the audio has come; on the first song, play that
    -- song again from its beginning.
    previous :: IO ()
  , -- | Play nothing, and forget the album — on the way out of the player, or
    -- out of the account.
    stop :: IO ()
  , -- | Halt the audio where it is.
    pause :: IO ()
  , -- | Let the audio run again from where it was held.
    resume :: IO ()
  , -- | Halt the audio if it is running, and let it run again if it is held.
    -- One key does both, so which of the two it is is asked of the backend
    -- rather than kept here as well. With nothing playing there is nothing to
    -- hold, and nothing happens.
    togglePause :: IO ()
  , -- | Move this many seconds through the song being played, forwards or
    -- back. The backend clamps it to that song, so a seek never reaches
    -- another one.
    seekBy :: Int -> IO ()
  , -- | The song being played, or nothing when the album has run out, a song
    -- was picked out of an unreachable server, or nothing has been picked
    -- yet.
    nowPlaying :: IO (Maybe Playing)
  , -- | Takes in everything the backend has said since it was last asked, and
    -- answers with the failures the player has to show.
    --
    -- This is where an album carries itself: a song running out starts the
    -- next one, and the album's last song running out ends the playing. A
    -- song that will not play is skipped — its failure is answered with, and
    -- the next song of the album starts — while a server that cannot be
    -- reached ends the playing, so that nothing further is started.
    attend :: IO [Failure]
  }
  deriving stock (Generic)

-- | The album position the session is on, and how it got there.
data Place = Place
  { queue :: Queue
  , arrival :: Arrival
  }
  deriving stock (Generic)

-- | A session over a backend, told where a song's audio lives — the address
-- the Subsonic client gives for a song id. Nothing is playing yet.
newSession :: Audio -> (SongId -> Text) -> IO Session
newSession audio address = do
  -- The album position is the one thing here that no rung above a cell holds:
  -- it outlives the call that sets it, so it is neither no state nor a fold;
  -- a model of it would have to carry a second copy of what the backend
  -- already says about the song being played, and the two could disagree; and
  -- only the one event loop ever reaches it, so there is nothing for STM to
  -- keep from interleaving.
  place <- newMVar Nothing
  let -- Puts the session on an album position and plays the song there, or,
      -- where the album has run out, leaves it playing nothing. Every change
      -- of what is playing goes through here.
      settle at = do
        case at of
          Nothing -> audio.stop
          Just here -> audio.play (trackOf address (view (#queue % #playing) here)) (Seconds 0)
        pure at

      -- Throws away whatever the backend has already said, because it is
      -- about the song that is being replaced. What it says of the song
      -- started in its place comes after.
      discard = do
        heard <- audio.nextEvent
        case heard of
          Nothing -> pure ()
          Just _ -> discard

      moveTo at = discard >> settle at

      advance = withPlace (\here -> settle (followed <$> forward here.queue))

      heed shown current = do
        heard <- audio.nextEvent
        case heard of
          Nothing -> pure (current, reverse shown)
          Just Finished -> advance current >>= heed shown
          Just (Failed failure) -> case failure of
            Unplayable _ -> advance current >>= heed (failure : shown)
            Unreachable _ -> settle Nothing >>= heed (failure : shown)

      onPlace = modifyMVar_ place

  pure
    Session
      { start = \queue -> onPlace $ \_ -> moveTo (Just (Place queue Picked))
      , next = onPlace $ withPlace $ \here -> moveTo (followed <$> forward here.queue)
      , previous = onPlace $ withPlace $ \here -> moveTo (Just (followed (backward here.queue)))
      , stop = onPlace $ \_ -> moveTo Nothing
      , pause = audio.pause
      , resume = audio.resume
      , togglePause = do
          state <- audio.nowPlaying
          case state of
            Stopped -> pure ()
            Loaded playback -> case playback.motion of
              Running -> audio.pause
              Paused -> audio.resume
      , seekBy = audio.seekBy
      , nowPlaying = do
          current <- readMVar place
          state <- audio.nowPlaying
          pure (playingAt state <$> current)
      , attend = modifyMVar place (heed [])
      }

playingAt :: State -> Place -> Playing
playingAt state place =
  Playing
    { song = view (#queue % #playing) place
    , elapsed = elapsedIn state
    , arrival = place.arrival
    , sound = soundIn state
    }

-- | An album position the session was moved to from another of its songs.
followed :: Queue -> Place
followed queue = Place queue Followed

trackOf :: (SongId -> Text) -> Song -> Track
trackOf address song =
  Track
    { url = address song.id
    , duration = song.duration
    }

-- | With no album in hand there is nothing to move through, so a control that
-- would move within one does nothing at all.
withPlace :: (Place -> IO (Maybe Place)) -> Maybe Place -> IO (Maybe Place)
withPlace act = maybe (pure Nothing) act
