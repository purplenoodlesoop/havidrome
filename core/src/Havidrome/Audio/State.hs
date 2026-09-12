-- | What the audio backend is doing, and what each control does to it: the
-- whole play\/pause\/stop machine and the spec's seek clamping, as pure
-- functions. Nothing here performs I\/O and nothing here knows about a
-- player, so all of it is exercised without an audio device.
--
-- A 'Command' is something that happened — the user asked for it, or the
-- player reported it. Stepping the state on one yields the 'Effect's the
-- player must be told about, and the 'Event's to hand back up.
module Havidrome.Audio.State
  ( -- * The track being played
    Track (..)

    -- * The state
  , State (..)
  , Playback (..)
  , Motion (..)
  , Phase (..)
  , initial

    -- * What is heard about it
  , Event (..)
  , Failure (..)

    -- * Moving it along
  , Command (..)
  , Effect (..)
  , step

    -- * Positions
  , clampTo
  ) where

import Data.Text as T (Text)
import GHC.Generics (Generic)
import Havidrome.Subsonic.Types (Seconds (..))

-- | One song's audio: where it is, and how long it runs. The duration is the
-- server's, the same one the overlay shows, and it is what a seek clamps
-- against.
data Track = Track
  { url :: Text
  , duration :: Seconds
  }
  deriving stock (Eq, Generic, Show)

-- | Whether a loaded track is moving or held where it is.
data Motion = Running | Paused
  deriving stock (Eq, Show)

-- | How far a loaded track has got towards its audio starting. A track is
-- loaded the moment the player is told to play it, but its audio starts only
-- once the player has fetched and opened it, which over a network takes a
-- while.
--
-- The player opening a track is heard before its audio starts, and anything
-- the player said before that is about a track it was playing earlier. So the
-- audio starting counts only once the opening has been heard: a start the
-- player reports late, about the track just replaced, is never taken for the
-- new one's.
data Phase
  = -- | The player has been told to play it, and has not yet said it is
    -- opening it.
    Requested
  | -- | The player is opening it, and its audio has not started.
    Opening
  | -- | Its audio has started. A held track can have begun: it is waiting at
    -- its position, and nothing is left to load.
    Begun
  deriving stock (Eq, Show)

-- | A loaded track, how it is moving, how far into it the audio has come, and
-- whether that audio has started yet.
data Playback = Playback
  { track :: Track
  , motion :: Motion
  , elapsed :: Seconds
  , phase :: Phase
  }
  deriving stock (Eq, Generic, Show)

-- | Either a track is loaded or nothing is. Nothing is loaded before the
-- first song, and after one is stopped, finishes, or fails.
data State
  = Stopped
  | Loaded Playback
  deriving stock (Eq, Show)

-- | Nothing loaded, nothing heard.
initial :: State
initial = Stopped

-- | Why a track stopped short of its end. The two are told apart because the
-- player above responds differently: an unreachable server ends playback,
-- while a file that will not play is skipped.
data Failure
  = -- | The server could not be reached for this track's audio.
    Unreachable Text
  | -- | The server was reached and the file still would not play:
    -- unsupported, or corrupt.
    Unplayable Text
  deriving stock (Eq, Show)

-- | Something the backend reports of its own accord. A track that was stopped
-- announces nothing, so 'Finished' means the audio really ran out, once.
data Event
  = Finished
  | Failed Failure
  deriving stock (Eq, Show)

-- | Everything that can move the state: the four controls, and what the
-- player says while it plays.
data Command
  = -- | Play this track, from this position into it.
    Start Track Seconds
  | Pause
  | Resume
  | -- | Seek this many seconds from where the track is now, forwards or back.
    SeekBy Int
  | Stop
  | -- | The player says the audio has reached this position.
    Observed Seconds
  | -- | The player says it is opening the track it was last told to play.
    Opened
  | -- | The player says the audio has started: after a track is opened, and
    -- again after every seek.
    Began
  | -- | The player says the track ran out.
    Ended
  | -- | The player says it could not play the track.
    Broke Failure
  deriving stock (Eq, Show)

-- | What the world outside must do about a step. Everything but 'Announce' is
-- an order for the player; 'Announce' is a report for the caller.
data Effect
  = -- | Play the audio at this address, beginning at this position.
    Load Text Seconds
  | -- | Hold the audio where it is, or let it run again.
    SetPaused Bool
  | -- | Move the audio to this position.
    SeekTo Seconds
  | -- | Play nothing.
    Unload
  | Announce Event
  deriving stock (Eq, Show)

-- | The state after a command, and what to do about it.
step :: Command -> State -> (State, [Effect])
step command state = case command of
  Start track from ->
    let at = clampTo track from
     in (Loaded (Playback track Running at Requested), [Load track.url at, SetPaused False])
  Pause -> onPlayback $ \playback -> case playback.motion of
    Running -> (Loaded playback {motion = Paused}, [SetPaused True])
    Paused -> (Loaded playback, [])
  Resume -> onPlayback $ \playback -> case playback.motion of
    Paused -> (Loaded playback {motion = Running}, [SetPaused False])
    Running -> (Loaded playback, [])
  SeekBy delta -> onPlayback $ \playback ->
    let at = clampTo playback.track (shiftBy delta playback.elapsed)
     in (Loaded playback {elapsed = at}, [SeekTo at])
  Stop -> onPlayback $ \_ -> (Stopped, [Unload])
  Observed at -> onPlayback $ \playback ->
    (Loaded playback {elapsed = clampTo playback.track at}, [])
  Opened -> onPlayback $ \playback -> case playback.phase of
    Requested -> (Loaded playback {phase = Opening}, [])
    _ -> (Loaded playback, [])
  -- Held or running, a track whose audio starts has begun: a track held
  -- while it loads finishes loading held.
  Began -> onPlayback $ \playback -> case playback.phase of
    Opening -> (Loaded playback {phase = Begun}, [])
    _ -> (Loaded playback, [])
  Ended -> onPlayback $ \_ -> (Stopped, [Announce Finished])
  Broke failure -> onPlayback $ \_ -> (Stopped, [Announce (Failed failure)])
 where
  -- With no track loaded there is nothing to pause, seek, stop or report, so
  -- everything the player says late — after a stop, or about the track it was
  -- just told to drop — falls here and is silently ignored.
  onPlayback act = case state of
    Stopped -> (Stopped, [])
    Loaded playback -> act playback

-- | The nearest position inside the track: never before its start, never past
-- its end. This is the whole of the spec's clamping, and the reason a seek
-- can never reach into another song.
clampTo :: Track -> Seconds -> Seconds
clampTo track (Seconds wanted) = Seconds (max 0 (min end wanted))
 where
  Seconds end = track.duration

shiftBy :: Int -> Seconds -> Seconds
shiftBy delta (Seconds at) = Seconds (at + delta)
