-- | Journals for the suite to hand to the handles it builds: values of the
-- player's own record type, writing to nothing or to somewhere a spec can read
-- back, so that no spec leaves a line in the journal of whoever ran it.
module Havidrome.Journal.Fake
  ( silent
  , recording
  ) where

import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Text (Text)
import Havidrome.Journal (Journal (Journal, file, writes))

-- | A journal that keeps nothing. It is what a spec about something else
-- hands over.
silent :: Journal
silent =
  Journal
    { file = pure "/dev/null"
    , writes = const (pure ())
    }

-- | A journal that keeps its lines in order, where a spec can read them.
recording :: IO (Journal, IORef [Text])
recording = do
  written <- newIORef []
  let keep said = atomicModifyIORef' written (\before -> (before <> [said], ()))
  pure (silent {writes = keep}, written)
