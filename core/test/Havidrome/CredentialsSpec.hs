-- | What the config file holds: one field per line, and every line read back
-- as it was written.
module Havidrome.CredentialsSpec (spec) where

import Data.Text qualified as T
import Havidrome.Credentials (Credentials (..), parse, render)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = describe "render and parse" $ do
  it "writes one field per line, in the clear" $
    render account
      `shouldBe` "server=https://music.example.com\nusername=someone\npassword=hunter2\n"

  prop "read back whatever was written, whatever it holds" $
    \(server', username', password') ->
      let credentials =
            Credentials
              { server = T.pack server'
              , username = T.pack username'
              , password = T.pack password'
              }
       in parse (render credentials) === Right credentials

account :: Credentials
account =
  Credentials
    { server = "https://music.example.com"
    , username = "someone"
    , password = "hunter2"
    }
