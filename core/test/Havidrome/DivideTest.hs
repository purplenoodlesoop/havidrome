-- | Division that answers for a divisor of zero instead of throwing on it.
module Havidrome.DivideTest (tests) where

import Havidrome.Check (example)
import Havidrome.Divide (quotient, quotientRemainder, remainder)
import Hedgehog (Gen, Group (Group), diff, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Divide"
    [
      ( "dividing by something gives the whole times it goes in, and what is left over"
      , example do
          quotientRemainder 7 2 === Just (3, 1)
          quotient 7 2 === Just 3
          remainder 7 2 === Just 1
      )
    ,
      ( "dividing rounds towards the lower number, so the remainder's sign follows the divisor"
      , example do
          quotientRemainder (-7) 2 === Just (-4, 1)
          quotientRemainder 7 (-2) === Just (-4, -1)
      )
    ,
      ( "the two halves of the answer put the number back together"
      , property do
          number <- forAll whole
          by <- forAll divisor
          fmap (\(times, left) -> times * by + left) (quotientRemainder number by) === Just number
      )
    ,
      ( "less is left over than the divisor"
      , property do
          number <- forAll whole
          by <- forAll divisor
          left <- maybe (fail "dividing by something gave nothing") pure (remainder number by)
          diff (abs left) (<) (abs by)
      )
    ,
      ( "dividing by nothing is nothing at all"
      , example do
          quotientRemainder 7 0 === Nothing
          quotient 7 0 === Nothing
          remainder 7 0 === Nothing
      )
    ,
      ( "dividing by nothing is nothing whatever the number"
      , property do
          number <- forAll whole
          quotientRemainder number 0 === Nothing
      )
    ]

-- | Any whole number, on either side of zero.
whole :: Gen Int
whole = Gen.int (Range.linearFrom 0 (-1000) 1000)

-- | Any whole number that can be divided by: every one but zero.
divisor :: Gen Int
divisor = Gen.filter (/= 0) whole
