{- | What a machine sends the player it takes to be playing: the media keys
it has of its own — the play\/pause, next and previous keys along the top of
a MacBook's keyboard — and the asks it makes of nobody's pressing.

A media key carries no meaning of its own. It is not a fourth way to pause,
a second way to skip: it stands for one of the keys the player already
binds, and the player does what that key does. The one thing that is the
media keys' alone is where they are pressed, which is anywhere on the
machine rather than on the terminal the player is drawn in — and that is
the shell's business, not this module's.

An ask the machine makes of its own accord is not a press of anything, so
it stands for no key of the player's and there is nothing here to say what
one would do: the player leaves it alone, and the two types are apart so
that leaving it alone is the only thing the player can be written to do.
-}
module Havidrome.Key.Media
  ( Media (..)
  , stands
  , Unbidden (..)
  ) where

import Havidrome.Key (Key (Character))

-- | A media key of the machine's own, pressed.
data Media
  = -- | Play\/pause, which stands for @space@.
    PlayPause
  | -- | Next, which stands for @n@.
    NextTrack
  | -- | Previous, which stands for @p@.
    PreviousTrack
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The key of the player's own that a media key stands for.
stands :: Media -> Key
stands = \case
  PlayPause -> Character ' '
  NextTrack -> Character 'n'
  PreviousTrack -> Character 'p'

{- | Something a machine asks of the player it takes to be playing, of its
own accord rather than because anyone pressed a key: macOS asks for a pause
when a FaceTime call starts, and Control Center and whatever is paired to
the machine ask for these too.
-}
data Unbidden
  = -- | Play, whatever the player is doing.
    Play
  | -- | Hold the audio, likewise.
    Pause
  | -- | Give up the audio altogether.
    Stop
  | -- | Move forward through what is playing, by the machine's own amount.
    SeekForward
  | -- | Move back through it, by the same.
    SeekBack
  deriving stock (Eq, Ord, Show, Enum, Bounded)
