{- | The margin every screen of the player is drawn inside, the login screen
and the browsing screen alike.
-}
module Havidrome.Margin (margined) where

import Brick (Widget, padAll)

{- | A screen drawn one cell in from the terminal's edges: a blank column at
the left and at the right, a blank row at the top and at the bottom, and
everything on the screen within them, however small the terminal is.
-}
margined :: Widget n -> Widget n
margined = padAll 1
