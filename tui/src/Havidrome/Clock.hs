{- | The clock the player reads, and the only place it reads one.

A moment is read through this record and then travels as a value: the beat
reads one and hands it to the strip, so nothing that only lays out a screen
is ever given a clock of its own to read.
-}
module Havidrome.Clock
  ( Clock (..)
  , HasClock (..)
  , mkClock
  ) where

import GHC.Clock (getMonotonicTime)
import Havidrome.Browse.Strip (Moment (Moment))

-- | What moment it is now.
newtype Clock = Clock
  { now :: IO Moment
  }

class HasClock env where
  getClock :: env -> Clock

{- | The runtime's monotonic clock, which is the one the strip is measured
against. It acquires nothing, so it is not in 'IO'.
-}
mkClock :: Clock
mkClock = Clock{now = Moment <$> getMonotonicTime}
