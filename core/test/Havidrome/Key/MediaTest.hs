{- | The media keys of a machine's own, and the keys of the player each one
stands for.
-}
module Havidrome.Key.MediaTest (tests) where

import Data.List (group, sort)
import Havidrome.Check (Checks, example)
import Havidrome.Key (Key (Character))
import Havidrome.Key.Media (Media (..), stands)
import Hedgehog (Group (Group), (===))

tests :: Group
tests = Group "Havidrome.Key.Media" (standing <> apart)

-- | Which key of the player's own each media key stands for.
standing :: Checks
standing =
  [
    ( "the play/pause key stands for space"
    , example (stands PlayPause === Character ' ')
    )
  ,
    ( "the next key stands for n"
    , example (stands NextTrack === Character 'n')
    )
  ,
    ( "the previous key stands for p"
    , example (stands PreviousTrack === Character 'p')
    )
  ]

-- | What holds of the three together.
apart :: Checks
apart =
  [
    ( "there are exactly three media keys"
    , example (length every === 3)
    )
  ,
    ( "no two of them stand for the same key"
    , example (length (group (sort (fmap stands every))) === length every)
    )
  ]

-- | Every media key there is.
every :: [Media]
every = [minBound ..]
