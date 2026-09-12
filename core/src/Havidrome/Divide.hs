-- | Dividing one whole number by another.
--
-- @div@, @mod@ and @divMod@ cannot say in their types that the divisor is not
-- zero, and throw when it is. Here that is answered instead: there is no
-- dividing by nothing, so the answer to it is nothing. This is the one module
-- the partial originals are used in, because it is what makes them total for
-- everywhere else.
module Havidrome.Divide
  ( quotient
  , remainder
  , quotientRemainder
  ) where

-- | How many whole times the second number goes into the first.
quotient :: Int -> Int -> Maybe Int
quotient number by = fst <$> quotientRemainder number by

-- | What is left of the first number once the second has gone into it as many
-- whole times as it does.
remainder :: Int -> Int -> Maybe Int
remainder number by = snd <$> quotientRemainder number by

-- | Both at once, which is one division rather than two.
quotientRemainder :: Int -> Int -> Maybe (Int, Int)
quotientRemainder number by
  | by == 0 = Nothing
  | otherwise = Just (number `divMod` by)
