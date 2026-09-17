{- | What the machine sends, as a capability of the player's: the media key
pressed from anywhere on the machine, and the press or the ask the player
makes itself.

A media key is not pressed on the terminal the player is drawn in — that is
the whole point of it — so no terminal can report one, and reading one is
no part of reading a key press. It arrives wherever the machine decides a
media key goes, and what is behind this record is the one thing of the
player's the machine will hand one to.

A machine that hands out no media keys presses nothing here, and the player
is then driven by its own keys alone.

What the machine asks of its own accord arrives by that same road, and this
record is where a test puts one on it. There is nothing to wait on for
those, because the player never hears one: an ask is answered where it
lands, by being dropped there.
-}
module Havidrome.Remote
  ( Remote (..)
  , HasRemote (..)

    -- * What it hears
  , module Havidrome.Key.Media
  ) where

import GHC.Generics (Generic)
import Havidrome.Key.Media

-- | The media keys, heard and pressed, and the machine's own asks, sent.
data Remote = Remote
  { awaits :: IO Media
  -- ^ The next media key pressed on the machine, waiting for one.
  , presses :: Media -> IO ()
  {- ^ Press one. Nothing in the player presses a media key — the machine
  is where they are pressed — so this is what a test presses one with,
  and what it presses is the very same key the machine would press.
  -}
  , sends :: Unbidden -> IO ()
  {- ^ Ask of the machine's own accord. Nothing in the player asks either,
  so this too is a test's, and what it sends arrives exactly where the
  machine's own ask arrives.
  -}
  }
  deriving stock (Generic)

class HasRemote env where
  getRemote :: env -> Remote
