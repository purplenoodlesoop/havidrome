module Main (main) where

import Havidrome (run)
import Havidrome.Subsonic.ClientSpec qualified as ClientSpec
import Havidrome.Subsonic.ProtocolSpec qualified as ProtocolSpec
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec $ do
  describe "Havidrome.run" $
    it "starts and exits" $
      run `shouldReturn` ()
  describe "Havidrome.Subsonic.Protocol" ProtocolSpec.spec
  describe "Havidrome.Subsonic" ClientSpec.spec
