module Main (main) where

import CredentialsSpec qualified
import Havidrome (run)
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec $ do
  describe "Havidrome.run" $
    it "starts and exits" $
      run `shouldReturn` ()
  CredentialsSpec.spec
