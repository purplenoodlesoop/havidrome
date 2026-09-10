module Main (main) where

import CredentialsSpec qualified
import Havidrome (run)
import Havidrome.Audio.IpcSpec qualified as IpcSpec
import Havidrome.Audio.StateSpec qualified as StateSpec
import Havidrome.AudioSpec qualified as AudioSpec
import Havidrome.Playback.QueueSpec qualified as QueueSpec
import Havidrome.PlaybackSpec qualified as PlaybackSpec
import Havidrome.Subsonic.ClientSpec qualified as ClientSpec
import Havidrome.Subsonic.ProtocolSpec qualified as ProtocolSpec
import Test.Hspec (describe, hspec, it, shouldReturn)

main :: IO ()
main = hspec $ do
  describe "Havidrome.run" $
    it "starts and exits" $
      run `shouldReturn` ()
  CredentialsSpec.spec
  describe "Havidrome.Subsonic.Protocol" ProtocolSpec.spec
  describe "Havidrome.Subsonic" ClientSpec.spec
  describe "Havidrome.Audio.State" StateSpec.spec
  describe "Havidrome.Audio.Ipc" IpcSpec.spec
  describe "Havidrome.Audio" AudioSpec.spec
  describe "Havidrome.Playback.Queue" QueueSpec.spec
  describe "Havidrome.Playback" PlaybackSpec.spec
