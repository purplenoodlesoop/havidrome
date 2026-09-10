module Main (main) where

import CredentialsSpec qualified
import Havidrome.Audio.IpcSpec qualified as IpcSpec
import Havidrome.Audio.StateSpec qualified as StateSpec
import Havidrome.AudioSpec qualified as AudioSpec
import Havidrome.Browse.ScreenSpec qualified as ScreenSpec
import Havidrome.Browse.StripSpec qualified as StripSpec
import Havidrome.BrowseSpec qualified as BrowseSpec
import Havidrome.LoginSpec qualified as LoginSpec
import Havidrome.Playback.QueueSpec qualified as QueueSpec
import Havidrome.PlaybackSpec qualified as PlaybackSpec
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
  describe "Havidrome.Audio.State" StateSpec.spec
  describe "Havidrome.Audio.Ipc" IpcSpec.spec
  describe "Havidrome.Audio" AudioSpec.spec
  describe "Havidrome.Playback.Queue" QueueSpec.spec
  describe "Havidrome.Playback" PlaybackSpec.spec
  describe "Havidrome.Browse" BrowseSpec.spec
  describe "Havidrome.Browse.Screen" ScreenSpec.spec
  describe "Havidrome.Browse.Strip" StripSpec.spec
  describe "Havidrome.Login" LoginSpec.spec
