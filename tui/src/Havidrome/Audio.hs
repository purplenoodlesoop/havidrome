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
  , HasAudio (..)
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
  , mkHttpReach
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
import Havidrome.Journal (HasJournal (getJournal), Journal (writes))
import Havidrome.Subsonic.Types (Seconds (..))
import Network.HTTP.Client (HttpException, Manager, httpNoBody, method, parseRequest)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.Socket (Family (AF_UNIX), Socket, SocketType (Stream), defaultProtocol, socketPair, socketToHandle)
import Optics.Core (view)
import Optics.Core qualified as Optics
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

class HasAudio env where
  getAudio :: env -> Audio

-- | An audio backend over the mpv on @PATH@, which the Nix build supplies.
-- The player is started when the action begins and gone when it ends.
withAudio :: (HasJournal env) => env -> (Audio -> IO a) -> IO a
withAudio env use = do
  reach <- mkHttpReach env
  withMpv env "mpv" [] reach use

-- | An audio backend over a named mpv, given extra options and a way to
-- probe the server. The tests use it to run mpv on a null audio output,
-- where there is no device to play to.
withMpv ::
  (HasJournal env) =>
  env ->
  FilePath ->
  [Text] ->
  Reach ->
  (Audio -> IO a) ->
  IO a
withMpv env program options reach use =
  bracket (start env program options reach) (end env) (use . audio env)

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
arguments :: [Text]
arguments =
  [ "--no-config"
  , "--no-terminal"
  , "--no-video"
  , "--idle=yes"
  , "--input-ipc-client=fd://0"
  ]

start :: (HasJournal env) => env -> FilePath -> [Text] -> Reach -> IO Mpv
start env program options reach = do
  (ours, theirs) <- socketPair AF_UNIX Stream defaultProtocol
  line <- socketToHandle ours ReadWriteMode
  hSetBuffering line LineBuffering
  process <- spawn program (arguments <> options) theirs
  state <- newMVar initial
  events <- newTChanIO
  let player = Player state events line reach
  send env player observePosition
  reader <- forkIO (drain env player)
  pure (Mpv player process reader)

spawn :: FilePath -> [Text] -> Socket -> IO ProcessHandle
spawn program options socket = do
  theirs <- socketToHandle socket ReadWriteMode
  (_, _, _, process) <-
    createProcess
      (proc program (fmap T.unpack options))
        { std_in = UseHandle theirs
        , std_out = NoStream
        , std_err = NoStream
        }
  pure process

end :: (HasJournal env) => env -> Mpv -> IO ()
end env mpv = do
  killThread mpv.reader
  send env mpv.player quit
  ignoringIO
    env
    "the line to the player would not close"
    (hClose (view (#player Optics.% #line) mpv))
  terminateProcess mpv.process
  _ <- waitForProcess mpv.process
  pure ()

audio :: (HasJournal env) => env -> Mpv -> Audio
audio env mpv =
  Audio
    { play = \track from -> perform env player (Start track from)
    , pause = perform env player Pause
    , resume = perform env player Resume
    , seekBy = perform env player . SeekBy
    , stop = perform env player Stop
    , nowPlaying = readMVar player.state
    , nextEvent = atomically (tryReadTChan player.events)
    , awaitEvent = atomically (readTChan player.events)
    }
 where
  player = mpv.player

-- | Moves the state on, and does what the step asks for. Holding the state
-- while the player is told keeps the orders in the order the state took them.
perform :: (HasJournal env) => env -> Player -> Command -> IO ()
perform env player command =
  modifyMVar_ player.state $ \current -> do
    let (next, effects) = step command current
    traverse_ (apply env player) effects
    pure next

apply :: (HasJournal env) => env -> Player -> Effect -> IO ()
apply env player effect = case effect of
  Announce event -> atomically (writeTChan player.events event)
  _ -> traverse_ (send env player) (render effect)

-- | A player that has died can no longer be told anything, and says so
-- through the reader instead of here.
send :: (HasJournal env) => env -> Player -> ByteString -> IO ()
send env player line =
  ignoringIO
    env
    "the player would not take an order"
    (ByteString.hPut player.line line >> hFlush player.line)

-- | Reads what mpv says for as long as it says anything. The end of the
-- stream is the player itself dying, which for a loaded track is a failure to
-- play it.
drain :: (HasJournal env) => env -> Player -> IO ()
drain env player = do
  line <- try (Char8.hGetLine player.line)
  case line of
    Left (fault :: IOException) -> do
      (getJournal env).writes ("the player stopped: " <> T.pack (show fault))
      broke env player "the player stopped"
    Right said -> do
      traverse_ (react env player) (readNotice said)
      drain env player

react :: (HasJournal env) => env -> Player -> Notice -> IO ()
react env player heard = case heard of
  Reached at -> perform env player (Observed at)
  Fetching -> perform env player Opened
  Underway -> perform env player Began
  RanOut -> perform env player Ended
  Broken detail -> broke env player detail

-- | Which of the two failures a broken track is, decided by asking the server
-- whether it is still there.
broke :: (HasJournal env) => env -> Player -> Text -> IO ()
broke env player detail = do
  current <- readMVar player.state
  case current of
    Stopped -> pure ()
    Loaded playback -> do
      answered <- view (#reach Optics.% #answers) player (view (#track Optics.% #url) playback)
      perform env player (Broke (failureOf answered detail))

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
mkHttpReach :: (HasJournal env) => env -> IO Reach
mkHttpReach env = Reach . probe env <$> newTlsManager

probe :: (HasJournal env) => env -> Manager -> Text -> IO Bool
probe env manager url = case parseRequest (T.unpack url) of
  Nothing -> pure False
  Just request -> do
    attempt <- try (httpNoBody request {method = "HEAD"} manager)
    case attempt of
      Left (fault :: HttpException) -> do
        (getJournal env).writes
          ("the server did not answer for " <> url <> ": " <> T.pack (show fault))
        pure False
      Right _ -> pure True

-- | Carries on from an exception the player can do nothing about, leaving the
-- journal to say what it was.
ignoringIO :: (HasJournal env) => env -> Text -> IO () -> IO ()
ignoringIO env about act = do
  attempt <- try act
  case attempt of
    Left (fault :: IOException) ->
      (getJournal env).writes (about <> ": " <> T.pack (show fault))
    Right () -> pure ()

