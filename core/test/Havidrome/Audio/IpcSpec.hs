-- | The lines in these tests are mpv's own, taken from a session with the
-- player this backend drives.
module Havidrome.Audio.IpcSpec (spec) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Havidrome.Audio.Ipc
import Havidrome.Audio.State (Effect (..), Event (..))
import Havidrome.Subsonic.Types (Seconds (..))
import Test.Hspec

-- | A rendered command, read back as JSON so the test does not depend on how
-- the fields happen to be laid out.
sent :: Effect -> Maybe Value
sent effect = render effect >>= Aeson.decodeStrict

spec :: Spec
spec = do
  speaking
  listening

speaking :: Spec
speaking = describe "what the player is told" $ do
  it "loads a track, replacing whatever plays, at the position asked for" $
    sent (Load "https://music.example.org/rest/stream?id=s1" (Seconds 42))
      `shouldBe` Aeson.decodeStrict
        "{\"command\":[\"loadfile\",\"https://music.example.org/rest/stream?id=s1\",\"replace\",-1,\"start=42\"]}"

  it "holds the audio" $
    sent (SetPaused True) `shouldBe` Aeson.decodeStrict "{\"command\":[\"set_property\",\"pause\",true]}"

  it "lets the audio run" $
    sent (SetPaused False) `shouldBe` Aeson.decodeStrict "{\"command\":[\"set_property\",\"pause\",false]}"

  it "seeks to a position, not by an amount: the clamping is already done" $
    sent (SeekTo (Seconds 35)) `shouldBe` Aeson.decodeStrict "{\"command\":[\"seek\",35,\"absolute\"]}"

  it "stops" $
    sent Unload `shouldBe` Aeson.decodeStrict "{\"command\":[\"stop\"]}"

  it "has nothing to say about a report for the caller" $
    render (Announce Finished) `shouldBe` Nothing

  it "ends every command with a newline, which is what separates them" $
    fmap ByteString.last (render Unload) `shouldBe` Just 10

  it "asks for the playing position to be reported" $
    (Aeson.decodeStrict observePosition :: Maybe Value)
      `shouldBe` Aeson.decodeStrict "{\"command\":[\"observe_property\",1,\"time-pos\"]}"

  it "asks the player to exit" $
    (Aeson.decodeStrict quit :: Maybe Value)
      `shouldBe` Aeson.decodeStrict "{\"command\":[\"quit\"]}"

listening :: Spec
listening = describe "what the player says" $ do
  it "reads a position, in whole seconds" $
    heard "{\"event\":\"property-change\",\"id\":1,\"name\":\"time-pos\",\"data\":2.577926}"
      `shouldBe` Just (Reached (Seconds 2))

  it "reads a position of none as no position at all" $
    heard "{\"event\":\"property-change\",\"id\":1,\"name\":\"time-pos\"}" `shouldBe` Nothing

  it "reads a track running out" $
    heard "{\"event\":\"end-file\",\"reason\":\"eof\",\"playlist_entry_id\":1}" `shouldBe` Just RanOut

  it "reads a file that would not play, in the player's words" $
    heard "{\"event\":\"end-file\",\"reason\":\"error\",\"playlist_entry_id\":2,\"file_error\":\"unrecognized file format\"}"
      `shouldBe` Just (Broken "unrecognized file format")

  it "reads a failure the player gave no reason for" $
    heard "{\"event\":\"end-file\",\"reason\":\"error\",\"playlist_entry_id\":2}"
      `shouldBe` Just (Broken "the player did not say why")

  it "hears nothing in a track it was told to stop" $
    heard "{\"event\":\"end-file\",\"reason\":\"stop\",\"playlist_entry_id\":5}" `shouldBe` Nothing

  it "reads the player starting on the track it was told to play" $
    heard "{\"event\":\"start-file\",\"playlist_entry_id\":1}" `shouldBe` Just Fetching

  it "reads the audio getting underway" $
    heard "{\"event\":\"playback-restart\"}" `shouldBe` Just Underway

  it "hears nothing in a track being loaded" $
    heard "{\"event\":\"file-loaded\"}" `shouldBe` Nothing

  it "hears nothing in the answer to a command" $
    heard "{\"data\":{\"playlist_entry_id\":1},\"request_id\":0,\"error\":\"success\"}" `shouldBe` Nothing

  it "hears nothing in a line that is not JSON at all" $
    heard "mpv fell over" `shouldBe` Nothing

heard :: ByteString -> Maybe Notice
heard = readNotice

