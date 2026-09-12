module Havidrome.Audio.StateSpec (spec) where

import Havidrome.Audio.State
import Havidrome.Subsonic.Types (Seconds (..))
import Test.Hspec
import Test.QuickCheck

-- | Three minutes of audio at a made-up address; long enough that a seek has
-- room on both sides of it.
track :: Track
track =
  Track {url = "https://music.example.org/rest/stream?id=s1", duration = Seconds 180}

-- | The track loaded, moving this way, this far along towards its audio
-- starting, and this far into it.
loaded :: Motion -> Phase -> Int -> State
loaded motion phase at = Loaded (Playback track motion (Seconds at) phase)

-- | The track just told to play, its audio not yet started.
requested :: Int -> State
requested = loaded Running Requested

-- | The track with its audio started, running or held.
running, paused :: Int -> State
running = loaded Running Begun
paused = loaded Paused Begun

-- | The state a run of commands leaves behind, from nothing playing.
stateAfter :: [Command] -> State
stateAfter = foldl' (\state command -> fst (step command state)) initial

-- | What the player is told when a command lands on a state.
told :: Command -> State -> [Effect]
told command = snd . step command

spec :: Spec
spec = do
  describe "starting" $ do
    it "plays a track from its beginning" $
      step (Start track (Seconds 0)) initial
        `shouldBe` (requested 0, [Load track.url (Seconds 0), SetPaused False])

    it "plays a track from a position inside it" $
      step (Start track (Seconds 42)) initial
        `shouldBe` (requested 42, [Load track.url (Seconds 42), SetPaused False])

    it "replaces whatever was playing, and keeps running" $
      stateAfter [Start track (Seconds 0), Pause, Start track (Seconds 5)] `shouldBe` requested 5

    it "replaces a track whose audio had started with one whose audio has not" $
      step (Start track (Seconds 0)) (running 30) `shouldBe` step (Start track (Seconds 0)) initial

    it "lets the audio run even where it was held before" $
      told (Start track (Seconds 0)) (paused 10)
        `shouldSatisfy` elem (SetPaused False)

  describe "pausing and resuming" $ do
    it "holds the audio where it is" $
      step Pause (running 30) `shouldBe` (paused 30, [SetPaused True])

    it "keeps the position while held" $
      stateAfter [Start track (Seconds 30), Opened, Began, Pause] `shouldBe` paused 30

    it "continues from that same position" $
      step Resume (paused 30) `shouldBe` (running 30, [SetPaused False])

    it "says nothing to the player when already held" $
      told Pause (paused 30) `shouldBe` []

    it "says nothing to the player when already running" $
      told Resume (running 30) `shouldBe` []

    it "has nothing to pause when nothing is playing" $
      step Pause initial `shouldBe` (Stopped, [])

  describe "the audio starting" $ do
    it "has not happened while the player is opening the track" $
      stateAfter [Start track (Seconds 0), Opened] `shouldBe` loaded Running Opening 0

    it "happens when the player says so, once it has said it opened the track" $
      stateAfter [Start track (Seconds 0), Opened, Began] `shouldBe` running 0

    it "is not taken from a start heard before the opening, which is the replaced track's" $
      stateAfter [Start track (Seconds 0), Began] `shouldBe` requested 0

    it "is not undone by the start the player reports after a seek" $
      stateAfter [Start track (Seconds 0), Opened, Began, SeekBy 5, Began] `shouldBe` running 5

    it "is not undone by a late opening either" $
      stateAfter [Start track (Seconds 0), Opened, Began, Opened] `shouldBe` running 0

    it "leaves a track held while it loaded held at its start" $
      stateAfter [Start track (Seconds 0), Pause, Opened, Began] `shouldBe` paused 0

    it "tells the player nothing, so a held track stays held" $ do
      told Opened (requested 0) `shouldBe` []
      told Began (loaded Paused Opening 0) `shouldBe` []

    it "is nobody's when nothing is playing" $ do
      step Opened initial `shouldBe` (Stopped, [])
      step Began initial `shouldBe` (Stopped, [])

  describe "the position the audio has reached" $ do
    it "follows what the player reports" $
      step (Observed (Seconds 12)) (running 3) `shouldBe` (running 12, [])

    it "never leaves the track, however the player counts" $
      step (Observed (Seconds 200)) (running 3) `shouldBe` (running 180, [])

    it "is nobody's when nothing is playing" $
      step (Observed (Seconds 12)) initial `shouldBe` (Stopped, [])

  describe "seeking" $ do
    it "moves the audio and the position together" $
      step (SeekBy 5) (running 30) `shouldBe` (running 35, [SeekTo (Seconds 35)])

    it "moves backwards by the same measure" $
      step (SeekBy (-30)) (running 60) `shouldBe` (running 30, [SeekTo (Seconds 30)])

    it "lands at the start rather than before it" $
      step (SeekBy (-30)) (running 10) `shouldBe` (running 0, [SeekTo (Seconds 0)])

    it "lands at the end rather than past it" $
      step (SeekBy 30) (running 175) `shouldBe` (running 180, [SeekTo (Seconds 180)])

    it "leaves a held track held" $
      step (SeekBy 5) (paused 30) `shouldBe` (paused 35, [SeekTo (Seconds 35)])

    it "has nothing to seek when nothing is playing" $
      step (SeekBy 5) initial `shouldBe` (Stopped, [])

    it "never leaves the track, wherever it is asked to go" $
      property $ \at delta duration ->
        let long = track {duration = Seconds (abs duration)}
            start = clampTo long (Seconds at)
            (sought, _) = step (SeekBy delta) (Loaded (Playback long Running start Begun))
         in case sought of
              Stopped -> False
              Loaded playback ->
                playback.elapsed >= Seconds 0
                  && playback.elapsed <= long.duration

  describe "stopping" $ do
    it "plays nothing afterwards" $
      step Stop (running 30) `shouldBe` (Stopped, [Unload])

    it "reports nothing: being stopped is not finishing" $
      told Stop (running 30) `shouldNotContain` [Announce Finished]

    it "has nothing to stop twice" $
      step Stop Stopped `shouldBe` (Stopped, [])

  describe "finishing" $ do
    it "is reported, once" $
      step Ended (running 180) `shouldBe` (Stopped, [Announce Finished])

    it "is not reported a second time" $
      told Ended (fst (step Ended (running 180))) `shouldBe` []

    it "is not reported for a track that was stopped" $
      told Ended (stateAfter [Start track (Seconds 0), Stop]) `shouldBe` []

  describe "failing" $ do
    it "reports a server it could not reach" $
      step (Broke (Unreachable "down")) (running 3)
        `shouldBe` (Stopped, [Announce (Failed (Unreachable "down"))])

    it "reports a file that will not play" $
      step (Broke (Unplayable "corrupt")) (running 3)
        `shouldBe` (Stopped, [Announce (Failed (Unplayable "corrupt"))])

    it "says nothing about a track already stopped" $
      told (Broke (Unplayable "corrupt")) Stopped `shouldBe` []
