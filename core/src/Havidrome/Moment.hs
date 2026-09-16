{- | A moment on the player's own clock, which is the one clock everything the
player times itself by is read off.

The clock is read in one place and a moment then travels as a value, so
nothing that only lays out a screen is given a clock to read. Two things are
timed against it: a line on the bottom strip with a few seconds to live,
which is the distance between two moments, and the turn the loading symbol
is on, which is the moment itself. It never goes backwards, so a line put
up at one moment is reliably taken down at a later one, and rows waiting at
the same moment are on the same turn whenever each of them began.
-}
module Havidrome.Moment
  ( Moment (..)
  , after
  ) where

-- | A moment, in seconds.
newtype Moment = Moment Double
  deriving stock (Eq, Ord, Show)

-- | The moment this many seconds after another.
after :: Double -> Moment -> Moment
after seconds (Moment at) = Moment (at + seconds)
