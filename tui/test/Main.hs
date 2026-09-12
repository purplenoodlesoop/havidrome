-- | The shell's suite: every group it holds, run one after another. The exit
-- status is hedgehog's own — non-zero the moment a group has a failing
-- property in it.
--
-- The groups that reach outside the process are run one property at a time:
-- the config file is found through the environment, which is the whole
-- process's, and the audio tests time a real player, which two of them at
-- once would throw off. The rest share nothing and are free to run together.
module Main (main) where

import Havidrome.AudioTest qualified as AudioTest
import Havidrome.Browse.ScreenTest qualified as ScreenTest
import Havidrome.Credentials.StoreTest qualified as StoreTest
import Havidrome.Key.VtyTest qualified as VtyTest
import Havidrome.Login.ScreenTest qualified as LoginScreenTest
import Havidrome.PlaybackTest qualified as PlaybackTest
import Havidrome.Subsonic.TransportTest qualified as TransportTest
import Havidrome.SubsonicTest qualified as SubsonicTest
import HavidromeTest qualified
import Hedgehog (checkParallel, checkSequential)
import Hedgehog.Main (defaultMain)

main :: IO ()
main =
  defaultMain $
    map
      checkSequential
      [ HavidromeTest.tests
      , StoreTest.tests
      , AudioTest.tests
      ]
      <> map
        checkParallel
        [ VtyTest.tests
        , SubsonicTest.tests
        , TransportTest.tests
        , PlaybackTest.tests
        , ScreenTest.tests
        , LoginScreenTest.tests
        ]
