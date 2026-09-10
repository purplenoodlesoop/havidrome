{-# LANGUAGE OverloadedStrings #-}

-- | An audio backend that makes no sound: it keeps the same state machine the
-- real one does, writes down every track it is told to play, and says only
-- what a test tells it to say.
--
-- Because the state machine is the real one, a stand-in answers a pause, a
-- seek and a position exactly as mpv-driven audio would, and a report about a
-- track that is no longer loaded is dropped here as it is there.
module Havidrome.Playback.Standin
  ( Standin
  , newStandin
  , standinAudio

    -- * A session over one
  , withStandin
  , address

    -- * What it was told
  , loaded
  , motionOf

    -- * What it says back
  , begin
  , reach
  , finish
  , breakWith
  ) where

import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Data.Foldable (traverse_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Text (Text)
import Havidrome.Audio (Audio (..))
import Havidrome.Audio.State
  ( Command (..)
  , Effect (..)
  , Event
  , Failure
  , Motion
  , Playback (..)
  , State (..)
  , initial
  , step
  )
import Havidrome.Playback (Session, newSession)
import Havidrome.Subsonic.Types (Seconds, SongId (SongId))

data Standin = Standin
  { standinState :: IORef State
  , standinLoaded :: IORef [(Text, Seconds)]
  , standinEvents :: TChan Event
  }

newStandin :: IO Standin
newStandin = do
  state <- newIORef initial
  played <- newIORef []
  events <- newTChanIO
  pure Standin {standinState = state, standinLoaded = played, standinEvents = events}

standinAudio :: Standin -> Audio
standinAudio standin =
  Audio
    { play = \track from -> perform standin (Start track from)
    , pause = perform standin Pause
    , resume = perform standin Resume
    , seekBy = perform standin . SeekBy
    , stop = perform standin Stop
    , nowPlaying = readIORef (standinState standin)
    , nextEvent = atomically (tryReadTChan (standinEvents standin))
    , awaitEvent = atomically (readTChan (standinEvents standin))
    }

-- | A session over a stand-in backend, and the stand-in behind it: the specs
-- ask the session for something and see what the backend was told.
withStandin :: (Standin -> Session -> IO a) -> IO a
withStandin use = do
  standin <- newStandin
  session <- newSession (standinAudio standin) address
  use standin session

-- | Where a song's audio lives, as the Subsonic client would say.
address :: SongId -> Text
address (SongId identifier) = "https://music.example.org/rest/stream?id=" <> identifier

-- | Every track it has been told to play, in the order it was told, each with
-- the position it was told to start at.
loaded :: Standin -> IO [(Text, Seconds)]
loaded = fmap reverse . readIORef . standinLoaded

-- | Whether the loaded track is running or held, and nothing when no track is
-- loaded.
motionOf :: Standin -> IO (Maybe Motion)
motionOf standin = do
  state <- readIORef (standinState standin)
  pure $ case state of
    Stopped -> Nothing
    Loaded playback -> Just (playbackMotion playback)

-- | The loaded track has been opened and its audio has started, as mpv says
-- it: the opening first, then the start.
begin :: Standin -> IO ()
begin standin = perform standin Opened >> perform standin Began

-- | The audio of the loaded track has come this far into it.
reach :: Standin -> Seconds -> IO ()
reach standin = perform standin . Observed

-- | The loaded track ran out.
finish :: Standin -> IO ()
finish standin = perform standin Ended

-- | The loaded track will not play, for this reason.
breakWith :: Standin -> Failure -> IO ()
breakWith standin = perform standin . Broke

perform :: Standin -> Command -> IO ()
perform standin command = do
  effects <- atomicModifyIORef' (standinState standin) (step command)
  traverse_ (record standin) effects

record :: Standin -> Effect -> IO ()
record standin effect = case effect of
  Load url from -> atomicModifyIORef' (standinLoaded standin) (\played -> ((url, from) : played, ()))
  Announce event -> atomically (writeTChan (standinEvents standin) event)
  _ -> pure ()
