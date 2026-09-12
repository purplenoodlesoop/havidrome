-- | The lines in these tests are mpv's own, taken from a session with the
-- player this backend drives.
module Havidrome.Audio.IpcTest (tests) where

import Data.Aeson (Value)
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Havidrome.Audio.Ipc
  ( Notice (Broken, Fetching, RanOut, Reached, Underway)
  , observePosition
  , quit
  , readNotice
  , render
  )
import Havidrome.Audio.State
  ( Effect (Announce, Load, SeekTo, SetPaused, Unload)
  , Event (Finished)
  )
import Havidrome.Check (example)
import Havidrome.Subsonic.Types (Seconds (..))
import Hedgehog (Group (Group), (===))

tests :: Group
tests =
  Group
    "Havidrome.Audio.Ipc"
    [
      ( "the player is told to load a track, replacing whatever plays, at the position asked for"
      , example
          ( sent (Load "https://music.example.org/rest/stream?id=s1" (Seconds 42))
              === Aeson.decodeStrict
                "{\"command\":[\"loadfile\",\"https://music.example.org/rest/stream?id=s1\",\"replace\",-1,\"start=42\"]}"
          )
      )
    ,
      ( "the player is told to hold the audio"
      , example
          ( sent (SetPaused True)
              === Aeson.decodeStrict "{\"command\":[\"set_property\",\"pause\",true]}"
          )
      )
    ,
      ( "the player is told to let the audio run"
      , example
          ( sent (SetPaused False)
              === Aeson.decodeStrict "{\"command\":[\"set_property\",\"pause\",false]}"
          )
      )
    ,
      ( "the player is told to seek to a position, not by an amount: the clamping is already done"
      , example
          (sent (SeekTo (Seconds 35)) === Aeson.decodeStrict "{\"command\":[\"seek\",35,\"absolute\"]}")
      )
    ,
      ( "the player is told to stop"
      , example (sent Unload === Aeson.decodeStrict "{\"command\":[\"stop\"]}")
      )
    ,
      ( "the player has nothing said to it about a report for the caller"
      , example (render (Announce Finished) === Nothing)
      )
    ,
      ( "every command ends in a newline, which is what separates them"
      , example (fmap ByteString.last (render Unload) === Just 10)
      )
    ,
      ( "the player is asked for the playing position to be reported"
      , example
          ( (Aeson.decodeStrict observePosition :: Maybe Value)
              === Aeson.decodeStrict "{\"command\":[\"observe_property\",1,\"time-pos\"]}"
          )
      )
    ,
      ( "the player is asked to exit"
      , example
          ( (Aeson.decodeStrict quit :: Maybe Value)
              === Aeson.decodeStrict "{\"command\":[\"quit\"]}"
          )
      )
    ,
      ( "a position is read in whole seconds"
      , example
          ( heard "{\"event\":\"property-change\",\"id\":1,\"name\":\"time-pos\",\"data\":2.577926}"
              === Just (Reached (Seconds 2))
          )
      )
    ,
      ( "a position of none is read as no position at all"
      , example
          ( heard "{\"event\":\"property-change\",\"id\":1,\"name\":\"time-pos\"}"
              === Nothing
          )
      )
    ,
      ( "a track running out is read"
      , example
          ( heard "{\"event\":\"end-file\",\"reason\":\"eof\",\"playlist_entry_id\":1}"
              === Just RanOut
          )
      )
    ,
      ( "a file that would not play is read, in the player's words"
      , example
          ( heard "{\"event\":\"end-file\",\"reason\":\"error\",\"playlist_entry_id\":2,\"file_error\":\"unrecognized file format\"}"
              === Just (Broken "unrecognized file format")
          )
      )
    ,
      ( "a failure the player gave no reason for is read"
      , example
          ( heard "{\"event\":\"end-file\",\"reason\":\"error\",\"playlist_entry_id\":2}"
              === Just (Broken "the player did not say why")
          )
      )
    ,
      ( "nothing is heard in a track the player was told to stop"
      , example
          ( heard "{\"event\":\"end-file\",\"reason\":\"stop\",\"playlist_entry_id\":5}"
              === Nothing
          )
      )
    ,
      ( "the player starting on the track it was told to play is read"
      , example
          (heard "{\"event\":\"start-file\",\"playlist_entry_id\":1}" === Just Fetching)
      )
    ,
      ( "the audio getting underway is read"
      , example (heard "{\"event\":\"playback-restart\"}" === Just Underway)
      )
    ,
      ( "nothing is heard in a track being loaded"
      , example (heard "{\"event\":\"file-loaded\"}" === Nothing)
      )
    ,
      ( "nothing is heard in the answer to a command"
      , example
          ( heard "{\"data\":{\"playlist_entry_id\":1},\"request_id\":0,\"error\":\"success\"}"
              === Nothing
          )
      )
    ,
      ( "nothing is heard in a line that is not JSON at all"
      , example (heard "mpv fell over" === Nothing)
      )
    ]

-- | A rendered command, read back as JSON so the test does not depend on how
-- the fields happen to be laid out.
sent :: Effect -> Maybe Value
sent effect = render effect >>= Aeson.decodeStrict

-- | What the backend makes of one line the player said.
heard :: ByteString -> Maybe Notice
heard = readNotice
