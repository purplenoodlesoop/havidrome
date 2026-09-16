{- | What the player is playing, as the now-playing overlay needs it: the song,
how far into it the audio has come, and whether its audio has started.

It is read off the audio backend's state and the album position, so there
is one answer to what is playing rather than two that can disagree.
-}
module Havidrome.Playback.Playing
  ( Playing (..)
  , Sound (..)
  , elapsedIn
  , soundIn
  ) where

import GHC.Generics (Generic)
import Havidrome.Audio.State (Motion, Phase (Begun), Playback (..), State (..))
import Havidrome.Subsonic.Types (Seconds (..), Song)

{- | Where a song's audio is: not started yet, or started and either running
or held. A song held while it loads is still loading until its audio starts,
and then it is held.
-}
data Sound
  = Loading
  | Sounding Motion
  deriving stock (Eq, Show)

{- | The song playing, how far into it the audio has come, and whether it is
still loading or its audio has started, running or held. Everything else the
now-playing overlay shows — the track name, the total time — is the song's
own.
-}
data Playing = Playing
  { song :: Song
  , elapsed :: Seconds
  , sound :: Sound
  }
  deriving stock (Eq, Generic, Show)

{- | How far the audio has come into what is loaded. With nothing loaded, it
has come nowhere.
-}
elapsedIn :: State -> Seconds
elapsedIn = \case
  Stopped -> Seconds 0
  Loaded playback -> playback.elapsed

{- | Whether the audio has started, and how it moves once it has. With nothing
loaded, no audio has.
-}
soundIn :: State -> Sound
soundIn = \case
  Stopped -> Loading
  Loaded playback
    | playback.phase == Begun -> Sounding playback.motion
    | otherwise -> Loading
