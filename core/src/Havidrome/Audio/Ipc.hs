{- | The half of the mpv conversation that is pure: the JSON line each
'Effect' is sent as, and the reading of the lines mpv sends back. Speaking
to a real player is the only thing left over, so what the player is told
and what it is understood to have said are both testable on their own.

mpv's IPC is one JSON object per line in each direction. Everything it says
that this player has no use for — replies to its own commands, the file
being loaded, the playlist going idle — reads as 'Nothing'.

mpv is also where the machine's own media keys land, so the lines that make
it hand one back rather than act on it, and the lines it hands one back in,
are here as well.
-}
module Havidrome.Audio.Ipc
  ( -- * Speaking
    render
  , observePosition
  , quit

    -- * The machine's media keys
  , mediaKeys
  , bindMedia
  , press

    -- * Listening
  , Notice (..)
  , readNotice
  ) where

import Data.Aeson (Value (Bool, Number), (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe, withObject)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as Lazy
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Audio.State (Effect (..))
import Havidrome.Key.Media (Media (..))
import Havidrome.Subsonic.Types (Seconds (..))

-- | The only things mpv says that the backend acts on.
data Notice
  = -- | The audio has reached this position, in whole seconds.
    Reached Seconds
  | {- | mpv has started on the file it was last told to play: it is fetching
    and opening it, and no audio has come of it yet.
    -}
    Fetching
  | {- | The audio is underway: the file has been opened and its audio has
    started, or a seek has landed. mpv says so even for a file held while
    it loads, once it is ready to play.
    -}
    Underway
  | -- | The track ran out on its own.
    RanOut
  | -- | The track will not play, in mpv's words.
    Broken Text
  | -- | A media key of the machine's own was pressed.
    Pressed Media
  deriving stock (Eq, Show)

{- | The line that sends an effect, or 'Nothing' for an effect that is not
mpv's business.
-}
render :: Effect -> Maybe ByteString
render effect = case effect of
  -- The @-1@ is where in the playlist the file goes; @replace@ makes that
  -- moot, but mpv wants the argument before the options it is given.
  Load url at ->
    Just (command [word "loadfile", word url, word "replace", Number (-1), word (startAt at)])
  SetPaused held -> Just (command [word "set_property", word "pause", Bool held])
  SeekTo (Seconds at) ->
    Just (command [word "seek", Number (fromIntegral at), word "absolute"])
  Unload -> Just (command [word "stop"])
  Announce _ -> Nothing

{- | Asks mpv to report the playing position as it changes, which is the only
thing it is asked to volunteer.
-}
observePosition :: ByteString
observePosition = command [word "observe_property", Number 1, word "time-pos"]

-- | Asks mpv to exit.
quit :: ByteString
quit = command [word "quit"]

{- | The keys mpv reports each media key of the machine's as, the first being
the one the player presses when it presses that media key itself.

mpv is where a media key arrives, because a machine hands its media keys to
whatever it takes to be playing, which is the mpv this backend drives and
not the terminal the player is drawn in. A machine that hands out no media
keys at all presses none of these, and the player is then driven by its own
keys alone.

A play\/pause key is one key, and the player toggles on it; a machine tells
the player it is playing or held, and sends whichever of the three that
makes right — the toggle, or the one that only plays, or the one that only
holds. All three are therefore that one key.
-}
mediaKeys :: Media -> NonEmpty Text
mediaKeys = \case
  PlayPause -> "PLAY" :| ["PLAYONLY", "PAUSEONLY"]
  NextTrack -> "NEXT" :| []
  PreviousTrack -> "PREV" :| []

{- | The lines that bind every key a media key arrives as, so that mpv hands
the press back here instead of acting on it: left to itself mpv would hold
its own audio, or skip its own playlist, behind the player's back, and the
player would go on saying what it last did.

A bound key says who bound it, so that a message another client of the same
player sends is not read as a key press.
-}
bindMedia :: [ByteString]
bindMedia =
  [ command [word "keybind", word key, word (message media)]
  | media <- [minBound ..]
  , key <- toList (mediaKeys media)
  ]

{- | The line that presses a media key inside mpv. Nothing in the player
presses one — a media key is pressed on the machine, and mpv is where it
lands — so this is here for the tests, which have no machine to press one
on and drive the whole of that path with it instead.
-}
press :: Media -> ByteString
press media = command [word "keypress", word name]
 where
  name :| _ = mediaKeys media

{- | What a bound key tells mpv to say, and what the player then reads a press
back out of.
-}
message :: Media -> Text
message media = "script-message " <> sender <> " " <> token media

{- | The name the player's own messages carry. mpv passes a message on to
every client it has, and this is what tells the player's own apart.
-}
sender :: Text
sender = "havidrome"

-- | What a message calls each media key.
token :: Media -> Text
token = \case
  PlayPause -> "play-pause"
  NextTrack -> "next"
  PreviousTrack -> "previous"

-- | The media key a message calls this, if it calls any.
tokened :: Text -> Maybe Media
tokened said = lookup said [(token media, media) | media <- [minBound ..]]

-- A word of a command, as the JSON string mpv reads it as.
word :: Text -> Value
word = Aeson.toJSON

command :: [Value] -> ByteString
command arguments =
  Lazy.toStrict (Aeson.encode (Aeson.object ["command" Aeson..= arguments])) <> "\n"

-- | The @start@ option of a @loadfile@: where in the track the audio begins.
startAt :: Seconds -> Text
startAt (Seconds at) = "start=" <> T.pack (show at)

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
    "client-message" -> do
      said <- event .: "args"
      case said :: [Text] of
        [from, what]
          | from == sender ->
              maybe (fail "a message of the player's that is no key press") (pure . Pressed) (tokened what)
        _ -> fail "a message another client of the player sent"
    _ -> fail "an event this player has no use for"

{- | mpv counts in fractions of a second; the player counts in whole ones, and
a position is the second the audio is inside.
-}
position :: Double -> Seconds
position = Seconds . floor
