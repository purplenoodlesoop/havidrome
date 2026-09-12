-- | Division that answers for a divisor of zero instead of throwing on it.
module Havidrome.DivideSpec (spec) where

import Havidrome.Divide (quotient, quotientRemainder, remainder)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonZero (NonZero), (===))

spec :: Spec
spec = do
  dividing
  byNothing

dividing :: Spec
dividing = describe "dividing by something" $ do
  it "gives the whole times it goes in, and what is left over" $ do
    quotientRemainder 7 2 `shouldBe` Just (3, 1)
    quotient 7 2 `shouldBe` Just 3
    remainder 7 2 `shouldBe` Just 1

  it "rounds towards the lower number, as the remainder's sign follows the divisor" $ do
    quotientRemainder (-7) 2 `shouldBe` Just (-4, 1)
    quotientRemainder 7 (-2) `shouldBe` Just (-4, -1)

  prop "puts the number back together from the two halves of the answer" $
    \number (NonZero by) ->
      fmap (\(whole, left) -> whole * by + left) (quotientRemainder number by)
        === Just number

  prop "leaves less over than the divisor" $
    \number (NonZero by) ->
      fmap ((< abs by) . abs) (remainder number by) === Just True

byNothing :: Spec
byNothing = describe "dividing by nothing" $ do
  it "is nothing at all" $ do
    quotientRemainder 7 0 `shouldBe` Nothing
    quotient 7 0 `shouldBe` Nothing
    remainder 7 0 `shouldBe` Nothing

  prop "is nothing whatever the number" $
    \number -> quotientRemainder number 0 === Nothing
