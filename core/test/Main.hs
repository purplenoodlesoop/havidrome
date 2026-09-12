module Main (main) where

import Havidrome.Audio.IpcSpec qualified as IpcSpec
import Havidrome.Audio.StateSpec qualified as StateSpec
import Havidrome.Browse.RowSpec qualified as RowSpec
import Havidrome.Browse.StripSpec qualified as StripSpec
import Havidrome.BrowseSpec qualified as BrowseSpec
import Havidrome.CredentialsSpec qualified as CredentialsSpec
import Havidrome.LoginSpec qualified as LoginSpec
import Havidrome.Playback.QueueSpec qualified as QueueSpec
import Havidrome.Subsonic.ProtocolSpec qualified as ProtocolSpec
import Havidrome.WidthSpec qualified as WidthSpec
import Test.Hspec (describe, hspec)

main :: IO ()
main = hspec $ do
  describe "Havidrome.Width" WidthSpec.spec
  describe "Havidrome.Credentials" CredentialsSpec.spec
  describe "Havidrome.Subsonic.Protocol" ProtocolSpec.spec
  describe "Havidrome.Audio.State" StateSpec.spec
  describe "Havidrome.Audio.Ipc" IpcSpec.spec
  describe "Havidrome.Playback.Queue" QueueSpec.spec
  describe "Havidrome.Browse" BrowseSpec.spec
  describe "Havidrome.Browse.Row" RowSpec.spec
  describe "Havidrome.Browse.Strip" StripSpec.spec
  describe "Havidrome.Login" LoginSpec.spec
