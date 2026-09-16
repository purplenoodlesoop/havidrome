{- | The clock the player reads, and the only place it reads one.

A moment is read through this record and then travels as a value: the beat
reads one and hands it to the screen, so nothing that only lays out a screen
is ever given a clock of its own to read. What it hands over is the core's
own moment, passed on from here so that whatever reads the clock takes the
moment from the same place.
-}
module Havidrome.Clock
  ( Clock (..)
  , HasClock (..)
  , mkClock

    -- * What it reads
  , Moment (..)
  ) where

import GHC.Clock (getMonotonicTime)
import Havidrome.Moment (Moment (Moment))

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
