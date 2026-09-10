{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The strip along the bottom of the browsing screen: one line under the
-- list, never more, and never a line the keyboard reaches. It is looked at
-- and nothing else, so the lists above it go on being walked exactly as they
-- were.
--
-- While a song is playing the strip is the now-playing overlay — the track
-- name, how far into it the audio has come, and how long it runs. With
-- nothing playing there is no line at all.
--
-- What goes wrong takes the strip over from the overlay for as long as it
-- has to be read, and the overlay is underneath it the whole time. How long
-- that is follows from what the failure did to the playing: a track that will
-- not play is skipped, so the next song's line is already waiting behind the
-- reason and takes the strip back after a few seconds; a server that cannot
-- be reached ends the playing, so there is no line waiting and the reason
-- stays until a key is pressed.
module Havidrome.Browse.Strip
  ( -- * The strip
    Strip
  , quiet

    -- * What is on it
  , Showing (..)
  , showing
  , overlaid
  , clock

    -- * What puts things on it
  , beat
  , wrong
  , pressed

    -- * The clock a few seconds are measured against
  , Moment (..)
  , moment
  ) where

import Data.Char (toUpper)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Clock (getMonotonicTime)
import Havidrome.Audio (Failure (Unplayable, Unreachable))
import Havidrome.Playback (Playing (playingElapsed, playingSong))
import Havidrome.Subsonic (Seconds (Seconds), Song (songDuration, songTitle))

-- | A moment on the player's own clock, in seconds. Only the distance between
-- two of them means anything: the clock never goes backwards, so a line put
-- up at one moment is reliably taken down at a later one.
newtype Moment = Moment Double
  deriving stock (Eq, Ord, Show)

-- | The moment it is now.
moment :: IO Moment
moment = Moment <$> getMonotonicTime

-- | The moment this many seconds after another.
after :: Double -> Moment -> Moment
after seconds (Moment at) = Moment (at + seconds)

-- | How long a skipped track's reason holds the strip before the next song's
-- line takes it back: the spec's few seconds.
briefly :: Double
briefly = 3

-- | The strip: the song the audio is on, if it is on one, and whatever is
-- being said over the top of it.
data Strip = Strip
  { stripPlaying :: Maybe Playing
  , stripSaid :: Maybe Said
  }
  deriving stock (Eq, Show)

-- | Something said over the overlay, and how long it stays there.
data Said = Said Text Life
  deriving stock (Eq, Show)

-- | How long a line stays on the strip.
data Life
  = -- | Until the next key press, which goes on to do its usual job: the
    -- strip takes no key of its own.
    UntilAKey
  | -- | Until this moment, when whatever is underneath takes the strip back.
    Until Moment
  deriving stock (Eq, Show)

-- | Nothing playing and nothing to say, which is no strip on screen at all.
quiet :: Strip
quiet = Strip {stripPlaying = Nothing, stripSaid = Nothing}

-- | What the strip has on it, and 'Nothing' when it has nothing and so is not
-- on screen.
data Showing
  = -- | What went wrong, in place of the overlay's usual contents.
    Wrong Text
  | -- | The now-playing overlay.
    Overlay Text
  deriving stock (Eq, Show)

showing :: Strip -> Maybe Showing
showing strip = case stripSaid strip of
  Just (Said said _) -> Just (Wrong said)
  Nothing -> Overlay . overlaid <$> stripPlaying strip

-- | The overlay's line: the track name, then how far into it the audio has
-- come and how long it runs.
overlaid :: Playing -> Text
overlaid playing =
  songTitle song <> "  " <> clock (playingElapsed playing) <> " / " <> clock (songDuration song)
  where
    song = playingSong playing

-- | A length of time as a clock reads it: minutes and seconds, and hours as
-- well once there are any.
clock :: Seconds -> Text
clock (Seconds total) = case hours of
  0 -> number minutes <> ":" <> pad seconds
  _ -> number hours <> ":" <> pad minutes <> ":" <> pad seconds
  where
    (hours, rest) = max 0 total `divMod` 3600
    (minutes, seconds) = rest `divMod` 60
    number = Text.pack . show
    pad = Text.justifyRight 2 '0' . number

-- | The strip a beat leaves behind: the song the audio is on now, and
-- whatever it failed at since the last beat.
--
-- A failure takes the strip over from the overlay, and the last of them
-- stands, being the one still worth reading. With none, a line whose time is
-- up comes down and what is underneath — by then the next song's line, or
-- nothing — has the strip back.
beat :: Moment -> Maybe Playing -> [Failure] -> Strip -> Strip
beat at playing failures strip =
  Strip
    { stripPlaying = playing
    , stripSaid = case failures of
        [] -> stripSaid strip >>= lasting at
        _ -> Just (saidOf at (last failures))
    }

-- | What a playback failure says, and for how long.
saidOf :: Moment -> Failure -> Said
saidOf at = \case
  Unplayable reason -> Said (sentence reason) (Until (after briefly at))
  Unreachable reason -> Said (sentence reason) UntilAKey

-- | The same line while its time is not up, and nothing once it is.
lasting :: Moment -> Said -> Maybe Said
lasting at said = case lifeOf said of
  UntilAKey -> Just said
  Until end
    | at < end -> Just said
    | otherwise -> Nothing

-- | Something the player itself has to say, which stays until a key press: a
-- level the library would not answer for.
wrong :: Text -> Strip -> Strip
wrong said strip = strip {stripSaid = Just (Said said UntilAKey)}

-- | The strip a key press leaves behind. What was waiting for a key goes;
-- what has a few seconds still to run keeps them, since it is the clock and
-- not the keyboard that takes it down.
pressed :: Strip -> Strip
pressed strip = strip {stripSaid = stripSaid strip >>= heard}
  where
    heard said = case lifeOf said of
      UntilAKey -> Nothing
      Until _ -> Just said

lifeOf :: Said -> Life
lifeOf (Said _ life) = life

-- | A reason as a sentence of its own: the layers underneath word them from
-- the middle of one.
sentence :: Text -> Text
sentence said = case Text.uncons said of
  Nothing -> said
  Just (opening, rest) -> Text.cons (toUpper opening) rest
