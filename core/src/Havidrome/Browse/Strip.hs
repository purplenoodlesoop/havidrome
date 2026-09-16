{- | The strip along the bottom of the browsing screen: one line under the
list, never more, and never a line the keyboard reaches. It is looked at
and nothing else, so the lists above it go on being walked exactly as they
were.

While a song is playing the strip is the now-playing overlay — whether its
audio runs or is held, the track name, a bar of how far into it the audio
has come, and the elapsed and total time. A song still loading reads the
same way, its bar empty and its elapsed time at nothing, however it came to
be playing. With nothing playing there is no line at all.

What goes wrong takes the strip over from the overlay for as long as it
has to be read, and the overlay is underneath it the whole time. How long
that is follows from what the failure did to the playing: a track that will
not play is skipped, so the next song's line is already waiting behind the
reason and takes the strip back after a few seconds; a server that cannot
be reached ends the playing, so there is no line waiting and the reason
stays until a key is pressed.
-}
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
  ) where

import Data.Maybe (fromMaybe)
import Data.Ord (clamp)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Audio.State
  ( Failure (Unplayable, Unreachable)
  , Motion (Paused, Running)
  , explain
  )
import Havidrome.Divide (quotient, quotientRemainder)
import Havidrome.Playback.Playing
  ( Playing (..)
  , Sound (Loading, Sounding)
  )
import Havidrome.Subsonic.Types (Seconds (Seconds), Song (..))
import Havidrome.Width qualified as Width

{- | A moment on the player's own clock, in seconds. Only the distance between
two of them means anything: the clock never goes backwards, so a line put
up at one moment is reliably taken down at a later one.
-}
newtype Moment = Moment Double
  deriving stock (Eq, Ord, Show)

-- | The moment this many seconds after another.
after :: Double -> Moment -> Moment
after seconds (Moment at) = Moment (at + seconds)

{- | How long a skipped track's reason holds the strip before the next song's
line takes it back: the spec's few seconds.
-}
briefly :: Double
briefly = 3

{- | The strip: the song the audio is on, if it is on one, whatever is being
said over the top of it, and the moment of the last beat, which is the clock
a line with a few seconds to live is measured against.
-}
data Strip = Strip
  { playing :: Maybe Playing
  , said :: Maybe Said
  , at :: Moment
  }
  deriving stock (Eq, Show)

-- | Something said over the overlay, and how long it stays there.
data Said = Said Text Life
  deriving stock (Eq, Show)

-- | How long a line stays on the strip.
data Life
  = {- | Until the next key press, which goes on to do its usual job: the
    strip takes no key of its own.
    -}
    UntilAKey
  | -- | Until this moment, when whatever is underneath takes the strip back.
    Until Moment
  deriving stock (Eq, Show)

-- | Nothing playing and nothing to say, which is no strip on screen at all.
quiet :: Strip
quiet = Strip{playing = Nothing, said = Nothing, at = Moment 0}

{- | What the strip has on it, and 'Nothing' when it has nothing and so is not
on screen.
-}
data Showing
  = -- | What went wrong, in place of the overlay's usual contents.
    Wrong Text
  | {- | The now-playing overlay for this song, which reads as 'overlaid' at
    whatever width the screen gives it.
    -}
    Overlay Playing
  deriving stock (Eq, Show)

showing :: Strip -> Maybe Showing
showing strip = case strip.said of
  Just (Said said _) -> Just (Wrong said)
  Nothing -> Overlay <$> strip.playing

{- | The overlay's line, laid out across this many columns: a symbol for
whether the audio runs or is held, the track name one space after it, a bar
of how far into it the audio has come, and the elapsed and total time.

The symbol, the name and the times are always there whole, and the bar
takes the width they leave between them. Once they leave none, there is no
bar, and the line runs on past the edge rather than onto a second one.

A song whose audio has not started neither runs nor is held, so its symbol's
column is blank, and nothing after it moves when the audio starts. The audio
has come nowhere into it yet, so its bar is empty and its elapsed time
stands at nothing; the line says no more than that about the loading,
however the song came to be playing.
-}
overlaid :: Int -> Playing -> Text
overlaid width playing =
  T.intercalate gap [titled, progress (width - taken) elapsed total, times]
 where
  song = playing.song
  titled = symbol playing.sound <> " " <> song.title
  elapsed = playing.elapsed
  total = song.duration
  times = clock elapsed <> " / " <> clock total
  gap = "  "
  taken = Width.text titled + Width.text times + 2 * Width.text gap

{- | What the overlay shows for where the audio is: one symbol while it runs, a
different one while it is held, and a blank of the same width while it has
not started.
-}
symbol :: Sound -> Text
symbol = \case
  Loading -> " "
  Sounding Running -> "⏵"
  Sounding Paused -> "⏸"

{- | A bar this many columns wide, filled for the part of the total that has
elapsed. Only whole columns fill, so the bar is empty until a column's worth
has played, and full only once all of it has. A total of nothing has no part
to be filled for, and its bar stays empty however long it runs.
-}
progress :: Int -> Seconds -> Seconds -> Text
progress width (Seconds elapsed) (Seconds total) =
  T.replicate filled "█" <> T.replicate (columns - filled) "░"
 where
  columns = max 0 width
  -- A total of nothing has nothing to divide into, and neither has one of
  -- less than nothing.
  filled = maybe 0 (clamp (0, columns)) (quotient (columns * elapsed) (max 0 total))

{- | A length of time as a clock reads it: minutes and seconds, and hours as
well once there are any.
-}
clock :: Seconds -> Text
clock (Seconds total) = case hours of
  0 -> number minutes <> ":" <> pad seconds
  _ -> number hours <> ":" <> pad minutes <> ":" <> pad seconds
 where
  (hours, rest) = fromMaybe (0, 0) (quotientRemainder (max 0 total) 3600)
  (minutes, seconds) = fromMaybe (0, 0) (quotientRemainder rest 60)
  number = T.pack . show
  pad = T.justifyRight 2 '0' . number

{- | The strip a beat leaves behind: the song the audio is on now, and
whatever it failed at since the last beat.

A failure takes the strip over from the overlay, and the last of them
stands, being the one still worth reading. With none, a line whose time is
up comes down and what is underneath — by then the next song's line, or
nothing — has the strip back.
-}
beat :: Moment -> Maybe Playing -> [Failure] -> Strip -> Strip
beat at playing failures strip =
  Strip
    { playing
    , said = case reverse failures of
        [] -> strip.said >>= lasting at
        latest : _ -> Just (saidOf at latest)
    , at
    }

-- | What a playback failure says, and for how long.
saidOf :: Moment -> Failure -> Said
saidOf at failure =
  Said (explain failure) $ case failure of
    Unplayable _ -> Until (after briefly at)
    Unreachable _ -> UntilAKey

-- | The same line while its time is not up, and nothing once it is.
lasting :: Moment -> Said -> Maybe Said
lasting at said = case lifeOf said of
  UntilAKey -> Just said
  Until end
    | at < end -> Just said
    | otherwise -> Nothing

{- | Something the player itself has to say, which stays until a key press: a
level the library would not answer for.
-}
wrong :: Text -> Strip -> Strip
wrong said strip = strip{said = Just (Said said UntilAKey)}

{- | The strip a key press leaves behind. What was waiting for a key goes;
what has a few seconds still to run keeps them, since it is the clock and
not the keyboard that takes it down.
-}
pressed :: Strip -> Strip
pressed strip = strip{said = strip.said >>= heard}
 where
  heard said = case lifeOf said of
    UntilAKey -> Nothing
    Until _ -> Just said

lifeOf :: Said -> Life
lifeOf (Said _ life) = life
