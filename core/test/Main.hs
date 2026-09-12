{- | The core's suite: every group it holds, run one after another. The exit
status is hedgehog's own — non-zero the moment a group has a failing
property in it.
-}
module Main (main) where

import Havidrome.Audio.IpcTest qualified as IpcTest
import Havidrome.Audio.StateTest qualified as StateTest
import Havidrome.Browse.RowTest qualified as RowTest
import Havidrome.Browse.StripTest qualified as StripTest
import Havidrome.BrowseTest qualified as BrowseTest
import Havidrome.CredentialsTest qualified as CredentialsTest
import Havidrome.DivideTest qualified as DivideTest
import Havidrome.LoginTest qualified as LoginTest
import Havidrome.Playback.QueueTest qualified as QueueTest
import Havidrome.Subsonic.ProtocolTest qualified as ProtocolTest
import Havidrome.Subsonic.TypesTest qualified as TypesTest
import Havidrome.WidthTest qualified as WidthTest
import Hedgehog (Group, checkParallel)
import Hedgehog.Main (defaultMain)

main :: IO ()
main = defaultMain (fmap checkParallel groups)

groups :: [Group]
groups =
  [ DivideTest.tests
  , WidthTest.tests
  , CredentialsTest.tests
  , TypesTest.tests
  , ProtocolTest.tests
  , StateTest.tests
  , IpcTest.tests
  , QueueTest.tests
  , BrowseTest.tests
  , RowTest.tests
  , StripTest.tests
  , LoginTest.tests
  ]
