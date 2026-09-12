-- | The keys the player is driven by, as its screens understand them. A press
-- really comes from a terminal, and reading one off a terminal is the shell's
-- business; a screen only ever sees what was pressed.
--
-- Only the keys the player binds are here. Anything else a terminal can
-- report — a function key, a mouse — is no t'Key' at all, and a screen given
-- none does nothing.
module Havidrome.Key
  ( Key (..)
  , Modifier (..)
  ) where

-- | A key that was pressed, a character key carrying the character it typed.
data Key
  = Character Char
  | Enter
  | Escape
  | Backspace
  | UpArrow
  | DownArrow
  | LeftArrow
  | RightArrow
  deriving stock (Eq, Ord, Show)

-- | A key held down along with another.
data Modifier
  = Shift
  | Ctrl
  | Meta
  | Alt
  deriving stock (Eq, Ord, Show, Enum, Bounded)
