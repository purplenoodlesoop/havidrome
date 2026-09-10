{-# LANGUAGE OverloadedStrings #-}

-- | The half of the mpv conversation that is pure: the JSON line each
-- 'Effect' is sent as, and the reading of the lines mpv sends back. Speaking
-- to a real player is the only thing left over, so what the player is told
-- and what it is understood to have said are both testable on their own.
--
-- mpv's IPC is one JSON object per line in each direction. Everything it says
-- that this player has no use for — replies to its own commands, the file
-- being loaded, the playlist going idle — reads as 'Nothing'.
module Havidrome.Audio.Ipc
  ( -- * Speaking
    render
  , observePosition
  , quit

    -- * Listening
  , Notice (..)
  , readNotice
  ) where

import Data.Aeson (Value (Bool, Number, String), (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe, withObject)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Audio.State (Effect (..))
import Havidrome.Subsonic.Types (Seconds (..))

-- | The only things mpv says that the backend acts on.
data Notice
  = -- | The audio has reached this position, in whole seconds.
    Reached Seconds
  | -- | mpv has started on the file it was last told to play: it is fetching
    -- and opening it, and no audio has come of it yet.
    Fetching
  | -- | The audio is underway: the file has been opened and its audio has
    -- started, or a seek has landed. mpv says so even for a file held while
    -- it loads, once it is ready to play.
    Underway
  | -- | The track ran out on its own.
    RanOut
  | -- | The track will not play, in mpv's words.
    Broken Text
  deriving stock (Eq, Show)

-- | The line that sends an effect, or 'Nothing' for an effect that is not
-- mpv's business.
render :: Effect -> Maybe ByteString
render effect = case effect of
  -- The @-1@ is where in the playlist the file goes; @replace@ makes that
  -- moot, but mpv wants the argument before the options it is given.
  Load url at ->
    Just (command [String "loadfile", String url, String "replace", Number (-1), String (startAt at)])
  SetPaused held -> Just (command [String "set_property", String "pause", Bool held])
  SeekTo at -> Just (command [String "seek", Number (fromIntegral (unSeconds at)), String "absolute"])
  Unload -> Just (command [String "stop"])
  Announce _ -> Nothing

-- | Asks mpv to report the playing position as it changes, which is the only
-- thing it is asked to volunteer.
observePosition :: ByteString
observePosition = command [String "observe_property", Number 1, String "time-pos"]

-- | Asks mpv to exit.
quit :: ByteString
quit = command [String "quit"]

command :: [Value] -> ByteString
command arguments =
  Lazy.toStrict (Aeson.encode (Aeson.object ["command" Aeson..= arguments])) <> "\n"

-- | The @start@ option of a @loadfile@: where in the track the audio begins.
startAt :: Seconds -> Text
startAt (Seconds at) = "start=" <> Text.pack (show at)

-- | What mpv just said, if it is anything this player acts on.
readNotice :: ByteString -> Maybe Notice
readNotice line = Aeson.decodeStrict line >>= parseMaybe notice

notice :: Value -> Parser Notice
notice = withObject "mpv event" $ \event -> do
  name <- event .: "event"
  case name :: Text of
    "property-change" -> do
      property <- event .: "name"
      case property :: Text of
        -- A property change with no value at all means mpv has none for it —
        -- between tracks, say — and says nothing about a position.
        "time-pos" -> Reached . position <$> event .: "data"
        _ -> fail "a property this player did not ask about"
    "start-file" -> pure Fetching
    "playback-restart" -> pure Underway
    "end-file" -> do
      reason <- event .: "reason"
      case reason :: Text of
        "eof" -> pure RanOut
        "error" -> Broken <$> event .:? "file_error" .!= "the player did not say why"
        -- Every other ending is one this player asked for: a stop, or a track
        -- replaced by the next one. Neither is news.
        _ -> fail "an ending this player asked for"
    _ -> fail "an event this player has no use for"

-- | mpv counts in fractions of a second; the player counts in whole ones, and
-- a position is the second the audio is inside.
position :: Double -> Seconds
position = Seconds . floor
