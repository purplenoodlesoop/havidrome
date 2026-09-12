-- | The backend against the player it really drives. mpv is run on a null
-- audio output, so these tests need no audio device — only the mpv the Nix
-- build supplies; without one they are left pending rather than failing.
module Havidrome.AudioSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as Lazy
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Word (Word32)
import Havidrome.Audio
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = do
  describe "telling the two failures apart" $ do
    it "blames the file when the server still answers" $
      failureOf True "unrecognized file format"
        `shouldBe` Unplayable "the file will not play: unrecognized file format"

    it "blames the network when it does not" $
      failureOf False "loading failed"
        `shouldBe` Unreachable "the server could not be reached: loading failed"

  describe "playing" $ do
    it "plays a track and reports it finished" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 4)
        finished <- waitForEvent audio
        finished `shouldBe` Just Finished

    it "reports the position advancing while it plays" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        moved <- waitUntil (fmap (>= Just (Seconds 1)) (reached audio))
        moved `shouldBe` True

    it "starts where it is told to, not at the beginning" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 3)
        moved <- waitUntil (fmap (>= Just (Seconds 3)) (reached audio))
        moved `shouldBe` True

  describe "the audio starting" $ do
    it "is reported once the track has been opened" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        started <- waitUntil (fmap (== Just Begun) (phase audio))
        started `shouldBe` True

    it "is reported for a track held while it loads, which stays held at its start" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        audio.pause
        started <- waitUntil (fmap (== Just Begun) (phase audio))
        started `shouldBe` True
        threadDelay 1500000
        reached audio `shouldReturn` Just (Seconds 0)
        audio.resume
        moved <- waitUntil (fmap (>= Just (Seconds 1)) (reached audio))
        moved `shouldBe` True

  describe "pausing" $
    it "freezes the position, and resuming continues from it" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        _ <- waitUntil (fmap (>= Just (Seconds 1)) (reached audio))
        audio.pause
        settle
        held <- reached audio
        threadDelay 1500000
        stillHeld <- reached audio
        stillHeld `shouldBe` held
        audio.resume
        moved <- waitUntil (fmap (> held) (reached audio))
        moved `shouldBe` True

  describe "seeking" $ do
    it "moves the audio and the reported position by that amount" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 1)
        audio.pause
        settle
        audio.seekBy 3
        reached audio `shouldReturn` Just (Seconds 4)

    it "lands at the start rather than before it" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 1)
        audio.pause
        settle
        audio.seekBy (-30)
        reached audio `shouldReturn` Just (Seconds 0)

    it "lands at the end rather than past it, and the track then finishes" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 1)
        audio.pause
        settle
        audio.seekBy 300
        reached audio `shouldReturn` Just track.duration
        audio.resume
        finished <- waitForEvent audio
        finished `shouldBe` Just Finished

  describe "stopping" $
    it "plays nothing more, and does not call that finishing" $
      withTone (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        _ <- waitUntil (fmap (>= Just (Seconds 1)) (reached audio))
        audio.stop
        quiet <- timeout 1500000 audio.awaitEvent
        quiet `shouldBe` Nothing
        audio.nowPlaying `shouldReturn` Stopped

  describe "failing" $ do
    it "reports a file that will not play as a play failure" $
      withGarbage (answering True) $ \audio track -> do
        audio.play track (Seconds 0)
        failed <- waitForEvent audio
        failed `shouldSatisfy` failing (Unplayable "the file will not play: unrecognized file format")

    it "reports a server it cannot reach as a network failure" $
      withGarbage (answering False) $ \audio track -> do
        audio.play track (Seconds 0)
        failed <- waitForEvent audio
        failed `shouldSatisfy` failing (Unreachable "the server could not be reached: unrecognized file format")

    it "asks the server itself, and calls a server that answers nothing a network failure" $ do
      reach <- httpReach
      withPlayer reach $ \audio -> do
        audio.play (Track nowhere (Seconds 60)) (Seconds 0)
        failed <- waitForEvent audio
        failed `shouldSatisfy` unreachable

-- | An invented address nothing answers on, so that a track fetched from it
-- fails the way a track fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://nowhere.example/stream"

-- | Six seconds of silence, which is long enough to seek about inside.
tone :: Seconds
tone = Seconds 6

withTone :: Reach -> (Audio -> Track -> IO ()) -> Expectation
withTone = withTrack (silence tone) tone

-- | Not audio at all, which mpv refuses to play.
withGarbage :: Reach -> (Audio -> Track -> IO ()) -> Expectation
withGarbage = withTrack "this is not a song" (Seconds 3)

-- | Runs an action against a real mpv over a track in a file of its own.
withTrack :: Lazy.ByteString -> Seconds -> Reach -> (Audio -> Track -> IO ()) -> Expectation
withTrack content duration reach use =
  withSystemTempDirectory "havidrome-audio" $ \dir -> do
    let file = dir </> "track"
    Lazy.writeFile file content
    withPlayer reach (\audio -> use audio (Track (T.pack file) duration))

-- | mpv on a null output, or a pending test where there is no mpv to run.
withPlayer :: Reach -> (Audio -> IO ()) -> Expectation
withPlayer reach use = do
  found <- findExecutable "mpv"
  case found of
    Nothing -> pendingWith "no mpv on PATH; the Nix build supplies one"
    Just mpv -> withMpv mpv ["--ao=null"] reach use

-- | A server that answers, or does not, whatever it is asked about.
answering :: Bool -> Reach
answering = Reach . const . pure

-- | How far into the track the audio has come, if anything is playing.
reached :: Audio -> IO (Maybe Seconds)
reached audio = fmap position audio.nowPlaying
 where
  position Stopped = Nothing
  position (Loaded playback) = Just playback.elapsed

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

failing :: Failure -> Maybe Event -> Bool
failing expected reported = reported == Just (Failed expected)

unreachable :: Maybe Event -> Bool
unreachable (Just (Failed (Unreachable _))) = True
unreachable _ = False

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
