-- | The lines in these tests are mpv's own, taken from a session with the
-- player this backend drives.
module Havidrome.Audio.IpcTest (tests) where

import Data.Aeson (Value (Number, String))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString qualified as ByteString
import Data.ByteString.Lazy qualified as LazyByteString
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Audio.Ipc
  ( Notice (Broken, Fetching, RanOut, Reached, Underway)
  , observePosition
  , quit
  , readNotice
  , render
  )
import Havidrome.Audio.State
  ( Effect (Announce, Load, SeekTo, SetPaused, Unload)
  , Event (Failed, Finished)
  , Failure (Unplayable, Unreachable)
  )
import Havidrome.Check (example)
import Havidrome.Subsonic.Types (Seconds (..))
import Hedgehog (Gen, Group (Group), PropertyT, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

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
    ,
      ( "every order for the player is a line, and the whole of one"
      , property do
          order <- forAll anyOrder
          traverse_ oneLine (render order)
          oneLine observePosition
          oneLine quit
      )
    ,
      ( "what the caller is told is no order for the player, whatever it is"
      , property do
          event <- forAll anyEvent
          render (Announce event) === Nothing
      )
    ,
      ( "a load names the track's address and the second it starts at, wherever that is"
      , property do
          url <- forAll anyUrl
          at <- forAll anySecond
          sent (Load url (Seconds at))
            === command
              [ String "loadfile"
              , String url
              , String "replace"
              , Number (-1)
              , String ("start=" <> Text.pack (show at))
              ]
      )
    ,
      ( "a seek names the second it is to land on, wherever that is"
      , property do
          at <- forAll anySecond
          sent (SeekTo (Seconds at))
            === command [String "seek", Number (fromIntegral at), String "absolute"]
      )
    ,
      ( "a position the player reports is the second the audio is inside, however finely it counts"
      , property do
          at <- forAll (Gen.double (Range.linearFrac 0 6000))
          heard (positionLine at) === Just (Reached (Seconds (floor at)))
      )
    ,
      ( "nothing is heard in an event this player never asked about"
      , property do
          name <- forAll (Gen.filter (`notElem` actedOn) (Gen.text (Range.linear 1 10) Gen.alphaNum))
          heard (eventLine name) === Nothing
      )
    ]

-- | A rendered command, read back as JSON so the test does not depend on how
-- the fields happen to be laid out.
sent :: Effect -> Maybe Value
sent effect = render effect >>= Aeson.decodeStrict

-- | What the backend makes of one line the player said.
heard :: ByteString -> Maybe Notice
heard = readNotice

-- | Whether a line the player is sent is one line: it ends the line it is,
-- and it is all of it.
oneLine :: ByteString -> PropertyT IO ()
oneLine line = do
  ByteString.count newline line === 1
  ByteString.last line === newline
  where
    newline = 10

-- | The command a rendered line carries, as JSON, beside what it should be.
command :: [Value] -> Maybe Value
command arguments = Just (Aeson.object ["command" Aeson..= arguments])

-- | Anything the state machine can order the player to do. What it announces
-- to the caller instead is 'anyEvent'.
anyOrder :: Gen Effect
anyOrder =
  Gen.choice
    [ Load <$> anyUrl <*> (Seconds <$> anySecond)
    , SetPaused <$> Gen.bool
    , SeekTo . Seconds <$> anySecond
    , pure Unload
    ]

anyEvent :: Gen Event
anyEvent =
  Gen.choice
    [ pure Finished
    , Failed <$> Gen.choice [Unreachable <$> reason, Unplayable <$> reason]
    ]
  where
    reason = Gen.text (Range.linear 0 12) Gen.unicode

-- | An address to play from, the characters a line has to escape among them.
anyUrl :: Gen Text
anyUrl = do
  song <- Gen.text (Range.linear 1 8) Gen.unicode
  pure ("https://music.example.org/rest/stream?id=" <> song)

anySecond :: Gen Int
anySecond = Gen.int (Range.linear 0 6000)

-- | The events this player acts on, which are the ones the line below is not
-- made of.
actedOn :: [Text]
actedOn = ["property-change", "start-file", "playback-restart", "end-file"]

-- | The line the player says a position in.
positionLine :: Double -> ByteString
positionLine at =
  saying
    [ "event" Aeson..= ("property-change" :: Text)
    , "id" Aeson..= (1 :: Int)
    , "name" Aeson..= ("time-pos" :: Text)
    , "data" Aeson..= at
    ]

-- | The line the player says something else in.
eventLine :: Text -> ByteString
eventLine name = saying ["event" Aeson..= name]

saying :: [Pair] -> ByteString
saying said = LazyByteString.toStrict (Aeson.encode (Aeson.object said))
