-- | The audio state machine: what each command does to what is playing, and
-- what the player is told about it.
module Havidrome.Audio.StateTest (tests) where

import Havidrome.Audio.State
  ( Command (Began, Broke, Ended, Observed, Opened, Pause, Resume, SeekBy, Start, Stop)
  , Effect (Announce, Load, SeekTo, SetPaused, Unload)
  , Event (Failed, Finished)
  , Failure (Unplayable, Unreachable)
  , Motion (Paused, Running)
  , Phase (Begun, Opening, Requested)
  , Playback (Playback, elapsed)
  , State (Loaded, Stopped)
  , Track (Track, duration, url)
  , clampTo
  , initial
  , step
  )
import Havidrome.Check (example)
import Havidrome.Subsonic.Types (Seconds (..))
import Hedgehog (Group (Group), diff, failure, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Audio.State"
    [
      ( "starting plays a track from its beginning"
      , example
          ( step (Start track (Seconds 0)) initial
              === (requested 0, [Load track.url (Seconds 0), SetPaused False])
          )
      )
    ,
      ( "starting plays a track from a position inside it"
      , example
          ( step (Start track (Seconds 42)) initial
              === (requested 42, [Load track.url (Seconds 42), SetPaused False])
          )
      )
    ,
      ( "starting replaces whatever was playing, and keeps running"
      , example
          ( stateAfter [Start track (Seconds 0), Pause, Start track (Seconds 5)]
              === requested 5
          )
      )
    ,
      ( "starting replaces a track whose audio had started with one whose audio has not"
      , example
          ( step (Start track (Seconds 0)) (running 30)
              === step (Start track (Seconds 0)) initial
          )
      )
    ,
      ( "starting lets the audio run even where it was held before"
      , example (diff (SetPaused False) elem (told (Start track (Seconds 0)) (paused 10)))
      )
    ,
      ( "pausing holds the audio where it is"
      , example (step Pause (running 30) === (paused 30, [SetPaused True]))
      )
    ,
      ( "pausing keeps the position while held"
      , example
          (stateAfter [Start track (Seconds 30), Opened, Began, Pause] === paused 30)
      )
    ,
      ( "resuming continues from that same position"
      , example (step Resume (paused 30) === (running 30, [SetPaused False]))
      )
    ,
      ( "pausing says nothing to the player when already held"
      , example (told Pause (paused 30) === [])
      )
    ,
      ( "resuming says nothing to the player when already running"
      , example (told Resume (running 30) === [])
      )
    ,
      ( "pausing has nothing to pause when nothing is playing"
      , example (step Pause initial === (Stopped, []))
      )
    ,
      ( "the audio starting has not happened while the player is opening the track"
      , example (stateAfter [Start track (Seconds 0), Opened] === loaded Running Opening 0)
      )
    ,
      ( "the audio starts when the player says so, once it has said it opened the track"
      , example (stateAfter [Start track (Seconds 0), Opened, Began] === running 0)
      )
    ,
      ( "the audio starting is not taken from a start heard before the opening, which is the replaced track's"
      , example (stateAfter [Start track (Seconds 0), Began] === requested 0)
      )
    ,
      ( "the audio starting is not undone by the start the player reports after a seek"
      , example
          ( stateAfter [Start track (Seconds 0), Opened, Began, SeekBy 5, Began]
              === running 5
          )
      )
    ,
      ( "the audio starting is not undone by a late opening either"
      , example
          (stateAfter [Start track (Seconds 0), Opened, Began, Opened] === running 0)
      )
    ,
      ( "the audio starting leaves a track held while it loaded held at its start"
      , example
          (stateAfter [Start track (Seconds 0), Pause, Opened, Began] === paused 0)
      )
    ,
      ( "the audio starting tells the player nothing, so a held track stays held"
      , example do
          told Opened (requested 0) === []
          told Began (loaded Paused Opening 0) === []
      )
    ,
      ( "the audio starting is nobody's when nothing is playing"
      , example do
          step Opened initial === (Stopped, [])
          step Began initial === (Stopped, [])
      )
    ,
      ( "the position the audio has reached follows what the player reports"
      , example (step (Observed (Seconds 12)) (running 3) === (running 12, []))
      )
    ,
      ( "the position never leaves the track, however the player counts"
      , example (step (Observed (Seconds 200)) (running 3) === (running 180, []))
      )
    ,
      ( "the position is nobody's when nothing is playing"
      , example (step (Observed (Seconds 12)) initial === (Stopped, []))
      )
    ,
      ( "seeking moves the audio and the position together"
      , example (step (SeekBy 5) (running 30) === (running 35, [SeekTo (Seconds 35)]))
      )
    ,
      ( "seeking moves backwards by the same measure"
      , example (step (SeekBy (-30)) (running 60) === (running 30, [SeekTo (Seconds 30)]))
      )
    ,
      ( "seeking lands at the start rather than before it"
      , example (step (SeekBy (-30)) (running 10) === (running 0, [SeekTo (Seconds 0)]))
      )
    ,
      ( "seeking lands at the end rather than past it"
      , example (step (SeekBy 30) (running 175) === (running 180, [SeekTo (Seconds 180)]))
      )
    ,
      ( "seeking leaves a held track held"
      , example (step (SeekBy 5) (paused 30) === (paused 35, [SeekTo (Seconds 35)]))
      )
    ,
      ( "seeking has nothing to seek when nothing is playing"
      , example (step (SeekBy 5) initial === (Stopped, []))
      )
    ,
      ( "seeking never leaves the track, wherever it is asked to go"
      , property do
          seconds <- forAll (Gen.int (Range.linear 0 600))
          at <- forAll (Gen.int (Range.linearFrom 0 (-300) 900))
          delta <- forAll (Gen.int (Range.linearFrom 0 (-900) 900))
          let long = track {duration = Seconds seconds}
              start = clampTo long (Seconds at)
          case fst (step (SeekBy delta) (Loaded (Playback long Running start Begun))) of
            Stopped -> failure
            Loaded playback -> do
              diff playback.elapsed (>=) (Seconds 0)
              diff playback.elapsed (<=) long.duration
      )
    ,
      ( "stopping plays nothing afterwards"
      , example (step Stop (running 30) === (Stopped, [Unload]))
      )
    ,
      ( "stopping reports nothing: being stopped is not finishing"
      , example (diff (Announce Finished) notElem (told Stop (running 30)))
      )
    ,
      ( "stopping has nothing to stop twice"
      , example (step Stop Stopped === (Stopped, []))
      )
    ,
      ( "finishing is reported, once"
      , example (step Ended (running 180) === (Stopped, [Announce Finished]))
      )
    ,
      ( "finishing is not reported a second time"
      , example (told Ended (fst (step Ended (running 180))) === [])
      )
    ,
      ( "finishing is not reported for a track that was stopped"
      , example (told Ended (stateAfter [Start track (Seconds 0), Stop]) === [])
      )
    ,
      ( "failing reports a server it could not reach"
      , example
          ( step (Broke (Unreachable "down")) (running 3)
              === (Stopped, [Announce (Failed (Unreachable "down"))])
          )
      )
    ,
      ( "failing reports a file that will not play"
      , example
          ( step (Broke (Unplayable "corrupt")) (running 3)
              === (Stopped, [Announce (Failed (Unplayable "corrupt"))])
          )
      )
    ,
      ( "failing says nothing about a track already stopped"
      , example (told (Broke (Unplayable "corrupt")) Stopped === [])
      )
    ]

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
stateAfter = foldl (\state command -> fst (step command state)) initial

-- | What the player is told when a command lands on a state.
told :: Command -> State -> [Effect]
told command = snd . step command
