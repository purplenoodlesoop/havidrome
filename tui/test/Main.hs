-- | The shell's suite. It is hspec today and hedgehog where the journal is
-- concerned; both are run, and a failure in either leaves the process with a
-- non-zero status.
module Main (main) where

import Havidrome.AudioSpec qualified as AudioSpec
import Havidrome.Browse.ScreenSpec qualified as ScreenSpec
import Havidrome.Credentials.StoreSpec qualified as StoreSpec
import Havidrome.JournalSpec qualified as JournalSpec
import Havidrome.Key.VtySpec qualified as VtySpec
import Havidrome.Login.ScreenSpec qualified as LoginScreenSpec
import Havidrome.PlaybackSpec qualified as PlaybackSpec
import Havidrome.Subsonic.TransportSpec qualified as TransportSpec
import Havidrome.SubsonicSpec qualified as SubsonicSpec
import HavidromeSpec qualified
import Hedgehog (checkSequential)
import Hedgehog.Main (defaultMain)
import Test.Hspec (describe)
import Test.Hspec.Runner (hspecResult, isSuccess)

-- | The groups run one after another: several of them give themselves a
-- directory by setting a variable the whole process shares, so no two of them
-- may be in flight at once.
main :: IO ()
main = defaultMain [specs, checkSequential JournalSpec.tests]

specs :: IO Bool
specs = fmap isSuccess . hspecResult $ do
  describe "Havidrome" HavidromeSpec.spec
  describe "Havidrome.Credentials.Store" StoreSpec.spec
  describe "Havidrome.Key.Vty" VtySpec.spec
  describe "Havidrome.Subsonic" SubsonicSpec.spec
  describe "Havidrome.Subsonic.Transport" TransportSpec.spec
  describe "Havidrome.Audio" AudioSpec.spec
  describe "Havidrome.Playback" PlaybackSpec.spec
  describe "Havidrome.Browse.Screen" ScreenSpec.spec
  describe "Havidrome.Login.Screen" LoginScreenSpec.spec
