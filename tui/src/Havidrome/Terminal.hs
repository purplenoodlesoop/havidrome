-- | The terminal the player is run in: the screens drawn on it, and the line
-- it is left with by a player that cannot go on.
--
-- A screen is a brick application, and running one is the whole of what the
-- player asks of a terminal: it is opened for that screen, driven until the
-- screen halts, and put down again. A run is one screen after another, and
-- none of them knows which terminal it was handed.
module Havidrome.Terminal
  ( Terminal (..)
  , HasTerminal (..)
  , mkTerminal
  , onTerminal
  ) where

import Brick (App, customMainWithDefaultVty)
import Brick.BChan (BChan)
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Graphics.Vty qualified as Vty
import System.IO (stderr)

-- | A terminal to put a screen on, and to say a last word on.
data Terminal = Terminal
  { runs :: forall s e n. (Ord n) => Maybe (BChan e) -> App s e n -> s -> IO s
  -- ^ Drive this application until it halts, and answer with the state it
  -- halted on. The channel, where there is one, is where the events the
  -- application raises for itself arrive.
  , says :: Text -> IO ()
  -- ^ Leave this line behind on the terminal.
  }

class HasTerminal env where
  getTerminal :: env -> Terminal

-- | The terminal the player was started in. It holds nothing open of its own
-- — every screen opens a terminal and puts it down again — so it is not in
-- 'IO'.
mkTerminal :: Terminal
mkTerminal =
  Terminal
    { runs = \events app state -> do
        -- brick hands back the terminal it was driving rather than putting it
        -- down, so that one screen can hand it to the next. The screens here
        -- hand it to nobody: each one that follows opens a terminal of its
        -- own, and a run that ends leaves the terminal as it found it.
        (final, vty) <- customMainWithDefaultVty events app state
        Vty.shutdown vty
        pure final
    , says = Text.IO.hPutStrLn stderr
    }

-- | Hands the terminal to a screen, and takes it back when that screen halts.
onTerminal :: (Ord n) => Terminal -> Maybe (BChan e) -> App s e n -> s -> IO s
onTerminal Terminal {runs} = runs
