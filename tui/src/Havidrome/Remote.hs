{- | The machine's own media keys, as a capability of the player's: the press
that arrives from anywhere on the machine, and the press the player makes
itself.

A media key is not pressed on the terminal the player is drawn in — that is
the whole point of it — so no terminal can report one, and reading one is
no part of reading a key press. It arrives wherever the machine decides a
media key goes, and what is behind this record is the one thing of the
player's the machine will hand one to.

A machine that hands out no media keys presses nothing here, and the player
is then driven by its own keys alone.
-}
module Havidrome.Remote
  ( Remote (..)
  , HasRemote (..)

    -- * The keys it hears
  , module Havidrome.Key.Media
  ) where

import GHC.Generics (Generic)
import Havidrome.Key.Media

-- | The media keys, heard and pressed.
data Remote = Remote
  { awaits :: IO Media
  -- ^ The next media key pressed on the machine, waiting for one.
  , presses :: Media -> IO ()
  {- ^ Press one. Nothing in the player presses a media key — the machine
  is where they are pressed — so this is what a test presses one with,
  and what it presses is the very same key the machine would press.
  -}
  }
  deriving stock (Generic)

class HasRemote env where
  getRemote :: env -> Remote
