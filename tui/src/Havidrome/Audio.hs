-- | The layer that actually produces sound: one song's audio, and the
-- controls over it. It knows a track's address and its length and nothing
-- else — not what album the track belongs to, not what plays next.
--
-- The sound is made by mpv, which the Nix build supplies and puts on the
-- player's @PATH@: nothing in Haskell decodes what a Navidrome library holds
-- — FLAC, Opus, whatever the file is — and puts it on an audio device, so the
-- spec's second choice, a player binary the build provides, is the one taken.
-- mpv is spoken to over its JSON IPC, on a socket handed to it as its standard
-- input, so no socket file is left anywhere on disk.
--
-- 'Audio' is a record of operations rather than a handle, so that everything
-- built on top of it can be exercised against a stand-in that makes no sound.
module Havidrome.Audio
  ( -- * The backend
    Audio (..)
  , withAudio
  , withMpv

    -- * What it plays, and what it says
  , Track (..)
  , State (..)
  , Playback (..)
  , Motion (..)
  , Phase (..)
  , Event (..)
  , Failure (..)
  , Seconds (..)

    -- * Telling the two failures apart
  , Reach (..)
  , httpReach
  , failureOf
  ) where

import Control.Concurrent (ThreadId, forkIO, killThread)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Concurrent.STM (TChan, atomically, newTChanIO, readTChan, tryReadTChan, writeTChan)
import Control.Exception (IOException, bracket, try)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Char8 qualified as Char8
import Data.Foldable (traverse_)
import Data.Text as T (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Havidrome.Audio.Ipc (Notice (..), observePosition, quit, readNotice, render)
import Havidrome.Audio.State
  ( Command (..)
  , Effect (..)
  , Event (..)
  , Failure (..)
  , Motion (..)
  , Phase (..)
  , Playback (..)
  , State (..)
  , Track (..)
  , initial
  , step
  )
import Havidrome.Subsonic.Types (Seconds (..))
import Network.HTTP.Client (HttpException, Manager, httpNoBody, method, parseRequest)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socketPair, socketToHandle)
import Optics.Core (view, (%))
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hSetBuffering)
import System.Process
  ( CreateProcess (std_err, std_in, std_out)
  , ProcessHandle
  , StdStream (NoStream, UseHandle)
  , createProcess
  , proc
  , terminateProcess
  , waitForProcess
  )

-- | Playing one song, and the controls the spec gives over it. Every
-- operation is immediate: none of them waits for the audio.
data Audio = Audio
  { -- | Play this track, beginning at this position into it — the start, or
    -- anywhere else inside it. Whatever was playing is replaced, and
    -- replacing a track is not it finishing.
    play :: Track -> Seconds -> IO ()
  , -- | Halt the audio where it is. The reported position freezes with it.
    pause :: IO ()
  , -- | Let the audio run again from where it was held.
    resume :: IO ()
  , -- | Move this many seconds through the current track, forwards or back,
    -- clamped to the track: never before its start, never past its end,
    -- never into another song.
    seekBy :: Int -> IO ()
  , -- | Play nothing. A stopped track reports no ending afterwards.
    stop :: IO ()
  , -- | What is loaded, whether it is mpv, and how far in the audio has
    -- come.
    nowPlaying :: IO State
  , -- | The next thing the backend has to report, if it has anything yet.
    nextEvent :: IO (Maybe Event)
  , -- | The next thing the backend has to report, waiting for it.
    awaitEvent :: IO Event
  }
  deriving stock (Generic)

-- | An audio backend over the mpv on @PATH@, which the Nix build supplies.
-- The player is started when the action begins and gone when it ends.
withAudio :: (Audio -> IO a) -> IO a
withAudio use = do
  reach <- httpReach
  withMpv "mpv" [] reach use

-- | An audio backend over a named mpv, given extra options and a way to
-- probe the server. The tests use it to run mpv on a null audio output,
-- where there is no device to play to.
withMpv :: FilePath -> [String] -> Reach -> (Audio -> IO a) -> IO a
withMpv program options reach use = bracket (start program options reach) end (use . audio)

-- | A mpv player: the state it is in, what it has to report, the line to
-- it, and the process and reader thread behind it.
data Player = Player
  { state :: MVar State
  , events :: TChan Event
  , line :: Handle
  , reach :: Reach
  }
  deriving stock (Generic)

data Mpv = Mpv
  { player :: Player
  , process :: ProcessHandle
  , reader :: ThreadId
  }
  deriving stock (Generic)

-- | How mpv is run: no window, no terminal, none of the user's own mpv
-- configuration, and its IPC on the socket it is given as standard input. It
-- stays idle between tracks instead of exiting.
arguments :: [String]
arguments =
  [ "--no-config"
  , "--no-terminal"
  , "--no-video"
  , "--idle=yes"
  , "--input-ipc-client=fd://0"
  ]

start :: FilePath -> [String] -> Reach -> IO Mpv
start program options reach = do
  (ours, theirs) <- socketPair AF_UNIX Stream defaultProtocol
  line <- socketToHandle ours ReadWriteMode
  hSetBuffering line LineBuffering
  process <- spawn program (arguments <> options) theirs
  state <- newMVar initial
  events <- newTChanIO
  let player = Player state events line reach
  send player observePosition
  reader <- forkIO (drain player)
  pure (Mpv player process reader)

spawn :: FilePath -> [String] -> Socket -> IO ProcessHandle
spawn program options socket = do
  theirs <- socketToHandle socket ReadWriteMode
  (_, _, _, process) <-
    createProcess
      (proc program options)
        { std_in = UseHandle theirs
        , std_out = NoStream
        , std_err = NoStream
        }
  pure process

end :: Mpv -> IO ()
end mpv = do
  killThread mpv.reader
  send mpv.player quit
  ignoringIO (hClose (view (#player % #line) mpv))
  terminateProcess mpv.process
  _ <- waitForProcess mpv.process
  pure ()

audio :: Mpv -> Audio
audio mpv =
  Audio
    { play = \track from -> perform player (Start track from)
    , pause = perform player Pause
    , resume = perform player Resume
    , seekBy = perform player . SeekBy
    , stop = perform player Stop
    , nowPlaying = readMVar player.state
    , nextEvent = atomically (tryReadTChan player.events)
    , awaitEvent = atomically (readTChan player.events)
    }
 where
  player = mpv.player

-- | Moves the state on, and does what the step asks for. Holding the state
-- while the player is told keeps the orders in the order the state took them.
perform :: Player -> Command -> IO ()
perform player command =
  modifyMVar_ player.state $ \current -> do
    let (next, effects) = step command current
    traverse_ (apply player) effects
    pure next

apply :: Player -> Effect -> IO ()
apply player effect = case effect of
  Announce event -> atomically (writeTChan player.events event)
  _ -> traverse_ (send player) (render effect)

-- | A player that has died can no longer be told anything, and says so
-- through the reader instead of here.
send :: Player -> ByteString -> IO ()
send player line =
  ignoringIO (ByteString.hPut player.line line >> hFlush player.line)

-- | Reads what mpv says for as long as it says anything. The end of the
-- stream is the player itself dying, which for a loaded track is a failure to
-- play it.
drain :: Player -> IO ()
drain player = do
  line <- try (Char8.hGetLine player.line)
  case line of
    Left (_ :: IOException) -> broke player "the player stopped"
    Right said -> do
      traverse_ (react player) (readNotice said)
      drain player

react :: Player -> Notice -> IO ()
react player heard = case heard of
  Reached at -> perform player (Observed at)
  Fetching -> perform player Opened
  Underway -> perform player Began
  RanOut -> perform player Ended
  Broken detail -> broke player detail

-- | Which of the two failures a broken track is, decided by asking the server
-- whether it is still there.
broke :: Player -> Text -> IO ()
broke player detail = do
  current <- readMVar player.state
  case current of
    Stopped -> pure ()
    Loaded playback -> do
      answered <- view (#reach % #answers) player (view (#track % #url) playback)
      perform player (Broke (failureOf answered detail))

-- | A server that still answers means the file itself is at fault; a server
-- that does not means the network is.
failureOf :: Bool -> Text -> Failure
failureOf answered detail
  | answered = Unplayable ("the file will not play: " <> detail)
  | otherwise = Unreachable ("the server could not be reached: " <> detail)

-- | Whether the server still answers for an address. It is asked only after
-- the player has failed, to tell the spec's network failure from its play
-- failure.
newtype Reach = Reach
  { answers :: Text -> IO Bool
  }
  deriving stock (Generic)

-- | A probe that asks the server for the track's headers and nothing else, so
-- that no audio is fetched to answer the question. Any answer at all, refusal
-- included, means the server was reached.
httpReach :: IO Reach
httpReach = do
  manager <- newTlsManager
  pure (Reach (probe manager))

probe :: Manager -> Text -> IO Bool
probe manager url = case parseRequest (T.unpack url) of
  Nothing -> pure False
  Just request -> do
    attempt <- try (httpNoBody request {method = "HEAD"} manager)
    pure $ case attempt of
      Left (_ :: HttpException) -> False
      Right _ -> True

ignoringIO :: IO () -> IO ()
ignoringIO act = do
  attempt <- try act
  case attempt of
    Left (_ :: IOException) -> pure ()
    Right () -> pure ()

