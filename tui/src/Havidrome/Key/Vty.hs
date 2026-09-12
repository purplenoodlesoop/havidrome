-- | Reading a key press off the terminal: what vty reports, as the key
-- vocabulary the screens are written against.
--
-- Only the keys the player binds have one; anything else vty can report — a
-- function key, a mouse, a key the player has no use for — has none, and a
-- screen given none does nothing.
module Havidrome.Key.Vty (pressed) where

import Graphics.Vty qualified as Vty
import Havidrome.Key (Key (..), Modifier (..))

-- | The press vty has reported, or nothing when it is one no screen binds.
pressed :: Vty.Key -> [Vty.Modifier] -> Maybe (Key, [Modifier])
pressed key modifiers = (,map held modifiers) <$> struck key

struck :: Vty.Key -> Maybe Key
struck = \case
  Vty.KChar character -> Just (Character character)
  Vty.KEnter -> Just Enter
  Vty.KEsc -> Just Escape
  Vty.KBS -> Just Backspace
  Vty.KUp -> Just UpArrow
  Vty.KDown -> Just DownArrow
  Vty.KLeft -> Just LeftArrow
  Vty.KRight -> Just RightArrow
  _ -> Nothing

held :: Vty.Modifier -> Modifier
held = \case
  Vty.MShift -> Shift
  Vty.MCtrl -> Ctrl
  Vty.MMeta -> Meta
  Vty.MAlt -> Alt
