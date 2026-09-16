{- | The media keys a machine has of its own — the play\/pause, next and
previous keys along the top of a MacBook's keyboard — and the keys of the
player each one stands for.

A media key carries no meaning of its own. It is not a fourth way to pause,
a second way to skip: it stands for one of the keys the player already
binds, and the player does what that key does. The one thing that is the
media keys' alone is where they are pressed, which is anywhere on the
machine rather than on the terminal the player is drawn in — and that is
the shell's business, not this module's.
-}
module Havidrome.Key.Media
  ( Media (..)
  , stands
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
