-- | What the config file holds: one field per line, and every line read back
-- as it was written.
module Havidrome.CredentialsTest (tests) where

import Data.Text (Text)
import Havidrome.Check (example)
import Havidrome.Credentials
  ( Credentials (..)
  , Fault (BadLine, MissingField, NotAccessible, RepeatedField)
  , explain
  , parse
  , render
  )
import Hedgehog (Gen, Group (Group), forAll, property, tripping, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Credentials"
    [
      ( "render writes one field per line, in the clear"
      , example
          ( render account
              === "server=https://music.example.com\nusername=someone\npassword=hunter2\n"
          )
      )
    ,
      ( "parse reads back whatever was rendered, whatever it holds"
      , property do
          credentials <- forAll anyCredentials
          tripping credentials render parse
      )
    ,
      ( "explain says what stopped the stored credentials being read, whichever fault it was"
      , example do
          explain (NotAccessible "permission denied")
            === "the stored credentials could not be read: NotAccessible \"permission denied\""
          explain (BadLine "server https://music.example.com")
            === "the stored credentials could not be read: BadLine \"server https://music.example.com\""
          explain (MissingField "password")
            === "the stored credentials could not be read: MissingField \"password\""
          explain (RepeatedField "username")
            === "the stored credentials could not be read: RepeatedField \"username\""
      )
    ]

account :: Credentials
account =
  Credentials
    { server = "https://music.example.com"
    , username = "someone"
    , password = "hunter2"
    }

-- | An account of any three strings at all, the newlines and backslashes the
-- file has to hide among them.
anyCredentials :: Gen Credentials
anyCredentials = do
  server <- field
  username <- field
  password <- field
  pure Credentials {server, username, password}
  where
    field :: Gen Text
    field = Gen.text (Range.linear 0 20) Gen.unicode
