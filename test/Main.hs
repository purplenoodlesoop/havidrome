module Main (main) where

import Havidrome (run)
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main =
  hspec $
    describe "Havidrome.run" $
      it "starts and exits" $
        run `shouldReturn` ()
