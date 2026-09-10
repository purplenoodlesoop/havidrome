module Main (main) where

import CredentialsSpec qualified
import Havidrome.Browse.ScreenSpec qualified as ScreenSpec
import Havidrome.BrowseSpec qualified as BrowseSpec
import Havidrome.Subsonic.ClientSpec qualified as ClientSpec
import Havidrome.Subsonic.ProtocolSpec qualified as ProtocolSpec
import HavidromeSpec qualified
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Havidrome" HavidromeSpec.spec
  CredentialsSpec.spec
  describe "Havidrome.Subsonic.Protocol" ProtocolSpec.spec
  describe "Havidrome.Subsonic" ClientSpec.spec
  describe "Havidrome.Browse" BrowseSpec.spec
  describe "Havidrome.Browse.Screen" ScreenSpec.spec
