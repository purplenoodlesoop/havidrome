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
-- Nothing here touches a terminal or a server, and the backend it drives is a
-- record of operations, so a session runs the whole album against a stand-in
-- that makes no sound.
module Havidrome.Playback
  ( -- * A session
    Session
  , newSession

    -- * Playing
  , start
  , next
  , previous
  , stop

    -- * The controls that do not change which song is playing
  , pause
  , resume
  , togglePause
  , seekBy

    -- * What it is doing, and what it has to say
  , Playing (..)
  , Arrival (..)
  , Sound (..)
  , nowPlaying
  , attend

    -- * The album being played
  , module Havidrome.Playback.Queue
  ) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Data.Text (Text)
import Havidrome.Audio
  ( Audio
  , Event (..)
  , Failure (..)
  , Motion (..)
  , Phase (..)
  , Playback (..)
  , State (..)
  , Track (..)
  )
import Havidrome.Audio qualified as Audio
import Havidrome.Playback.Queue
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId)

-- | An album being played through one audio backend. It holds the album, the
-- place in it and how the session came to that place; how far into a song the
-- audio has come is the backend's to say, and is asked of it rather than kept
-- here twice.
data Session = Session
  { sessionAudio :: Audio
  , sessionAddress :: SongId -> Text
  , sessionPlace :: MVar (Maybe Place)
  }

-- | The album position the session is on, and how it got there.
data Place = Place
  { placeQueue :: Queue
  , placeArrival :: Arrival
  }

-- | How the song playing came to be the one playing.
data Arrival
  = -- | It was picked: the album was started from it.
    Picked
  | -- | The album came to it from another of its songs, by running on to it
    -- or by being moved through with next or previous.
    Followed
  deriving stock (Eq, Show)

-- | A session over a backend, told where a song's audio lives — the address
-- the Subsonic client gives for a song id. Nothing is playing yet.
newSession :: Audio -> (SongId -> Text) -> IO Session
newSession audio address = do
  place <- newMVar Nothing
  pure Session {sessionAudio = audio, sessionAddress = address, sessionPlace = place}

-- | The song playing, how far into it the audio has come, how it came to be
-- playing, and whether it is still loading or its audio has started, running
-- or held. Everything else the now-playing overlay shows — the track name, the
-- total time — is the song's own.
data Playing = Playing
  { playingSong :: Song
  , playingElapsed :: Seconds
  , playingArrival :: Arrival
  , playingSound :: Sound
  }
  deriving stock (Eq, Show)

-- | Where a song's audio is: not started yet, or started and either running
-- or held. A song held while it loads is still loading until its audio starts,
-- and then it is held.
data Sound
  = Loading
  | Sounding Motion
  deriving stock (Eq, Show)

-- | Play this album from the song it is on, in place of whatever was playing.
start :: Session -> Queue -> IO ()
start session queue = onPlace session $ \_ -> do
  discard session
  settle session (Just (Place queue Picked))

-- | Play the next song of the album; on the last song, end the playing.
next :: Session -> IO ()
next session = onPlace session $ withPlace $ \place -> do
  discard session
  settle session (followed <$> forward (placeQueue place))

-- | Play the previous song of the album, from its beginning, however far into
-- the current song the audio has come; on the first song, play that song
-- again from its beginning.
previous :: Session -> IO ()
previous session = onPlace session $ withPlace $ \place -> do
  discard session
  settle session (Just (followed (backward (placeQueue place))))

-- | Play nothing, and forget the album — on the way out of the player, or out
-- of the account.
stop :: Session -> IO ()
stop session = onPlace session $ \_ -> do
  discard session
  settle session Nothing

-- | Halt the audio where it is.
pause :: Session -> IO ()
pause = Audio.pause . sessionAudio

-- | Let the audio run again from where it was held.
resume :: Session -> IO ()
resume = Audio.resume . sessionAudio

-- | Halt the audio if it is running, and let it run again if it is held.
-- One key does both, so which of the two it is is asked of the backend rather
-- than kept here as well. With nothing playing there is nothing to hold, and
-- nothing happens.
togglePause :: Session -> IO ()
togglePause session = do
  state <- Audio.nowPlaying (sessionAudio session)
  case state of
    Stopped -> pure ()
    Loaded playback -> case playbackMotion playback of
      Running -> pause session
      Paused -> resume session

-- | Move this many seconds through the song being played, forwards or back.
-- The backend clamps it to that song, so a seek never reaches another one.
seekBy :: Session -> Int -> IO ()
seekBy session = Audio.seekBy (sessionAudio session)

-- | The song being played, or nothing when the album has run out, a song was
-- picked out of an unreachable server, or nothing has been picked yet.
nowPlaying :: Session -> IO (Maybe Playing)
nowPlaying session = do
  current <- readMVar (sessionPlace session)
  state <- Audio.nowPlaying (sessionAudio session)
  pure (playingAt state <$> current)

playingAt :: State -> Place -> Playing
playingAt state place =
  Playing
    { playingSong = playing (placeQueue place)
    , playingElapsed = elapsedIn state
    , playingArrival = placeArrival place
    , playingSound = soundIn state
    }

elapsedIn :: State -> Seconds
elapsedIn state = case state of
  Stopped -> Seconds 0
  Loaded playback -> playbackElapsed playback

-- | Whether the audio has started, and how it moves once it has. With nothing
-- loaded, no audio has.
soundIn :: State -> Sound
soundIn state = case state of
  Stopped -> Loading
  Loaded playback
    | playbackPhase playback == Begun -> Sounding (playbackMotion playback)
    | otherwise -> Loading

-- | Takes in everything the backend has said since it was last asked, and
-- answers with the failures the player has to show.
--
-- This is where an album carries itself: a song running out starts the next
-- one, and the album's last song running out ends the playing. A song that
-- will not play is skipped — its failure is answered with, and the next song
-- of the album starts — while a server that cannot be reached ends the
-- playing, so that nothing further is started.
attend :: Session -> IO [Failure]
attend session = modifyMVar (sessionPlace session) (heed [])
 where
  heed shown current = do
    heard <- Audio.nextEvent (sessionAudio session)
    case heard of
      Nothing -> pure (current, reverse shown)
      Just Finished -> advance current >>= heed shown
      Just (Failed failure) -> case failure of
        Unplayable _ -> advance current >>= heed (failure : shown)
        Unreachable _ -> settle session Nothing >>= heed (failure : shown)
  advance = withPlace (\place -> settle session (followed <$> forward (placeQueue place)))

-- | Puts the session on an album position and plays the song there, or, where
-- the album has run out, leaves it playing nothing. Every change of what is
-- playing goes through here.
settle :: Session -> Maybe Place -> IO (Maybe Place)
settle session place = do
  case place of
    Nothing -> Audio.stop (sessionAudio session)
    Just at -> Audio.play (sessionAudio session) (trackOf session (playing (placeQueue at))) (Seconds 0)
  pure place

-- | An album position the session was moved to from another of its songs.
followed :: Queue -> Place
followed queue = Place queue Followed

trackOf :: Session -> Song -> Track
trackOf session song =
  Track
    { trackUrl = sessionAddress session (songId song)
    , trackDuration = songDuration song
    }

-- | Throws away whatever the backend has already said, because it is about
-- the song that is being replaced. What it says of the song started here
-- comes after.
discard :: Session -> IO ()
discard session = do
  heard <- Audio.nextEvent (sessionAudio session)
  case heard of
    Nothing -> pure ()
    Just _ -> discard session

onPlace :: Session -> (Maybe Place -> IO (Maybe Place)) -> IO ()
onPlace session act = modifyMVar_ (sessionPlace session) act

-- | With no album in hand there is nothing to move through, so a control that
-- would move within one does nothing at all.
withPlace :: (Place -> IO (Maybe Place)) -> Maybe Place -> IO (Maybe Place)
withPlace act = maybe (pure Nothing) act
