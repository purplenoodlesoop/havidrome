module Main (main) where

import Havidrome.AudioSpec qualified as AudioSpec
import Havidrome.Browse.ScreenSpec qualified as ScreenSpec
import Havidrome.Credentials.StoreSpec qualified as StoreSpec
import Havidrome.Key.VtySpec qualified as VtySpec
import Havidrome.Login.ScreenSpec qualified as LoginScreenSpec
import Havidrome.PlaybackSpec qualified as PlaybackSpec
import Havidrome.Subsonic.TransportSpec qualified as TransportSpec
import Havidrome.SubsonicSpec qualified as SubsonicSpec
import HavidromeSpec qualified
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Havidrome" HavidromeSpec.spec
  describe "Havidrome.Credentials.Store" StoreSpec.spec
  describe "Havidrome.Key.Vty" VtySpec.spec
  describe "Havidrome.Subsonic" SubsonicSpec.spec
  describe "Havidrome.Subsonic.Transport" TransportSpec.spec
  describe "Havidrome.Audio" AudioSpec.spec
  describe "Havidrome.Playback" PlaybackSpec.spec
  describe "Havidrome.Browse.Screen" ScreenSpec.spec
  describe "Havidrome.Login.Screen" LoginScreenSpec.spec
