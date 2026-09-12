-- | The backend against the player it really drives. mpv is run on a null
-- audio output, so these tests need no audio device — only the mpv the Nix
-- build supplies; without one they say so and pass rather than failing.
--
-- Each of them drives a real process through one scenario, so each is
-- genuinely one case. What takes an input at all is the wording a failure
-- carries, and that is stated over generated input.
module Havidrome.AudioTest (tests) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Havidrome.Audio
import Havidrome.Check (Checks, example)
import Hedgehog (Group (Group), PropertyT, annotate, assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)

tests :: Group
tests =
  Group
    "Havidrome.Audio"
    ( blame
        <> running
        <> holding
        <> seeks
        <> stopping
        <> complaints
    )

-- | Which of the two failures a complaint from the player is read as.
blame :: Checks
blame =
  [
    ( "blames the file when the server still answers"
    , example do
        failureOf True "unrecognized file format"
          === Unplayable "the file will not play: unrecognized file format"
    )
  ,
    ( "blames the network when it does not"
    , example do
        failureOf False "loading failed"
          === Unreachable "the server could not be reached: loading failed"
    )
  ,
    ( "blames the one or the other by whether the server answered, whatever went wrong"
    , property do
        detail <- forAll (Gen.text (Range.linear 0 40) Gen.unicode)
        failureOf True detail === Unplayable ("the file will not play: " <> detail)
        failureOf False detail === Unreachable ("the server could not be reached: " <> detail)
    )
  ]

-- | A track playing, and the position and the phase it reports.
running :: Checks
running =
  [
    ( "plays a track and reports it finished"
    , example . driving (playing (Seconds 4) (\audio _ -> waitForEvent audio)) $ \finished ->
        finished === Just Finished
    )
  ,
    ( "reports the position advancing while it plays"
    , example . driving (playing (Seconds 0) (\audio _ -> reaches audio (Seconds 1))) $ assert
    )
  ,
    ( "starts where it is told to, not at the beginning"
    , example . driving (playing (Seconds 3) (\audio _ -> reaches audio (Seconds 3))) $ assert
    )
  ,
    ( "reports the audio starting once the track has been opened"
    , example . driving (playing (Seconds 0) (\audio _ -> begun audio)) $ assert
    )
  ,
    ( "reports it for a track held while it loads, which stays held at its start"
    , example
        . driving
          ( playing (Seconds 0) $ \audio _ -> do
              audio.pause
              started <- begun audio
              threadDelay 1500000
              held <- reached audio
              audio.resume
              moved <- reaches audio (Seconds 1)
              pure (started, held, moved)
          )
        $ \(started, held, moved) -> do
          assert started
          held === Just (Seconds 0)
          assert moved
    )
  ]

-- | A pause, which freezes the position until the audio runs on.
holding :: Checks
holding =
  [
    ( "freezes the position on a pause, and resuming continues from it"
    , example
        . driving
          ( playing (Seconds 0) $ \audio _ -> do
              _ <- reaches audio (Seconds 1)
              audio.pause
              settle
              held <- reached audio
              threadDelay 1500000
              stillHeld <- reached audio
              audio.resume
              moved <- waitUntil (fmap (> held) (reached audio))
              pure (held, stillHeld, moved)
          )
        $ \(held, stillHeld, moved) -> do
          stillHeld === held
          assert moved
    )
  ]

-- | A seek, and the ends of the track it stops at.
seeks :: Checks
seeks =
  [
    ( "moves the audio and the reported position by the amount it is seeked"
    , example . driving (seeking 3 (\audio _ -> reached audio)) $ \there ->
        there === Just (Seconds 4)
    )
  ,
    ( "lands at the start rather than before it"
    , example . driving (seeking (-30) (\audio _ -> reached audio)) $ \there ->
        there === Just (Seconds 0)
    )
  ,
    ( "lands at the end rather than past it, and the track then finishes"
    , example
        . driving
          ( seeking 300 $ \audio track -> do
              there <- reached audio
              audio.resume
              finished <- waitForEvent audio
              pure (there, track.duration, finished)
          )
        $ \(there, ending, finished) -> do
          there === Just ending
          finished === Just Finished
    )
  ]

-- | The audio stopped, which is not the track finishing.
stopping :: Checks
stopping =
  [
    ( "plays nothing more once stopped, and does not call that finishing"
    , example
        . driving
          ( playing (Seconds 0) $ \audio _ -> do
              _ <- reaches audio (Seconds 1)
              audio.stop
              quiet <- timeout 1500000 audio.awaitEvent
              (,) quiet <$> audio.nowPlaying
          )
        $ \(quiet, left) -> do
          quiet === Nothing
          left === Stopped
    )
  ]

-- | What the player is told about a track it cannot play at all.
complaints :: Checks
complaints =
  [
    ( "reports a file that will not play as a play failure"
    , example . driving (garbage True (\audio _ -> waitForEvent audio)) $ \failed ->
        failed === Just (Failed (Unplayable "the file will not play: unrecognized file format"))
    )
  ,
    ( "reports a server it cannot reach as a network failure"
    , example . driving (garbage False (\audio _ -> waitForEvent audio)) $ \failed ->
        failed
          === Just (Failed (Unreachable "the server could not be reached: unrecognized file format"))
    )
  ,
    ( "asks the server itself, and calls a server that answers nothing a network failure"
    , example
        . driving
          ( do
              reach <- mkHttpReach
              withPlayer reach $ \audio -> do
                audio.play (Track nowhere (Seconds 60)) (Seconds 0)
                waitForEvent audio
          )
        $ \failed -> assert (case failed of Just (Failed (Unreachable _)) -> True; _ -> False)
    )
  ]

-- | Checks what a real player did, or says there was none to drive and lets
-- the check pass: outside Nix there may be no mpv, and inside it there always
-- is one.
driving :: IO (Maybe a) -> (a -> PropertyT IO ()) -> PropertyT IO ()
driving scenario check =
  evalIO scenario >>= maybe (annotate "no mpv on PATH; the Nix build supplies one") check

-- | An invented address nothing answers on, so that a track fetched from it
-- fails the way a track fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://nowhere.example/stream"

-- | Six seconds of silence, which is long enough to seek about inside.
tone :: Seconds
tone = Seconds 6

-- | A tone playing from this point on, and what a scenario made of it.
playing :: Seconds -> (Audio -> Track -> IO a) -> IO (Maybe a)
playing = withTrack (silence tone) tone (answering True)

-- | A tone held one second in and seeked by this much, and what a scenario
-- made of it. It is held first so that the position a seek leaves is the one
-- read back, and not one the audio has moved past.
seeking :: Int -> (Audio -> Track -> IO a) -> IO (Maybe a)
seeking by use = playing (Seconds 1) $ \audio track -> do
  audio.pause
  settle
  audio.seekBy by
  use audio track

-- | Not audio at all, which mpv refuses to play, against a server that
-- answers or does not.
garbage :: Bool -> (Audio -> Track -> IO a) -> IO (Maybe a)
garbage answers = withTrack "this is not a song" (Seconds 3) (answering answers) (Seconds 0)

-- | Runs a scenario against a real mpv over a track in a file of its own,
-- told to play it from this point on.
withTrack ::
  Lazy.ByteString ->
  Seconds ->
  Reach ->
  Seconds ->
  (Audio -> Track -> IO a) ->
  IO (Maybe a)
withTrack content duration reach from use =
  withSystemTempDirectory "havidrome-audio" $ \dir -> do
    let file = dir </> "track"
    Lazy.writeFile file content
    let track = Track (T.pack file) duration
    withPlayer reach $ \audio -> audio.play track from >> use audio track

-- | mpv on a null output, or nothing at all where there is no mpv to run.
withPlayer :: Reach -> (Audio -> IO a) -> IO (Maybe a)
withPlayer reach use =
  findExecutable "mpv" >>= \case
    Nothing -> pure Nothing
    Just mpv -> Just <$> withMpv mpv ["--ao=null"] reach use

-- | A server that answers, or does not, whatever it is asked about.
answering :: Bool -> Reach
answering = Reach . const . pure

-- | How far into the track the audio has come, if anything is playing.
reached :: Audio -> IO (Maybe Seconds)
reached audio = fmap position audio.nowPlaying
 where
  position Stopped = Nothing
  position (Loaded playback) = Just playback.elapsed

-- | Whether the audio gets this far into the track before the waiting is up.
reaches :: Audio -> Seconds -> IO Bool
reaches audio mark = waitUntil (fmap (>= Just mark) (reached audio))

-- | Whether the loaded track's audio is reported as started before the
-- waiting is up.
begun :: Audio -> IO Bool
begun audio = waitUntil (fmap (== Just Begun) (phase audio))

-- | How far the loaded track has got towards its audio starting, if anything
-- is loaded.
phase :: Audio -> IO (Maybe Phase)
phase audio = fmap phaseOf audio.nowPlaying
 where
  phaseOf Stopped = Nothing
  phaseOf (Loaded playback) = Just playback.phase

-- | The next thing the backend reports, or nothing if it stays silent for
-- long enough that it never will.
waitForEvent :: Audio -> IO (Maybe Event)
waitForEvent audio = timeout 10000000 audio.awaitEvent

-- | Waits for something to come true, giving up after ten seconds.
waitUntil :: IO Bool -> IO Bool
waitUntil check = attempt (200 :: Int)
 where
  attempt 0 = pure False
  attempt left = do
    now <- check
    if now then pure True else threadDelay 50000 >> attempt (left - 1)

-- | Long enough for a position the player already sent to arrive, so that a
-- test reads the position a pause left and not one from before it.
settle :: IO ()
settle = threadDelay 300000

-- | A playable file of silence: unheard even where there is an audio device,
-- and understood by any player without a library to decode it.
silence :: Seconds -> Lazy.ByteString
silence (Seconds seconds) =
  Builder.toLazyByteString $
    Builder.string7 "RIFF"
      <> Builder.word32LE (fromIntegral (36 + samples))
      <> Builder.string7 "WAVE"
      <> Builder.string7 "fmt "
      <> Builder.word32LE 16
      <> Builder.word16LE 1 -- uncompressed
      <> Builder.word16LE 1 -- one channel
      <> Builder.word32LE bytesASecond
      <> Builder.word32LE bytesASecond
      <> Builder.word16LE 1 -- one byte a sample
      <> Builder.word16LE 8
      <> Builder.string7 "data"
      <> Builder.word32LE (fromIntegral samples)
      <> Builder.lazyByteString (Lazy.replicate (fromIntegral samples) 128)
 where
  rate = 8000 :: Int
  samples = rate * seconds
  bytesASecond = fromIntegral rate :: Word32
