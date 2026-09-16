{- | The player itself: the credentials a run starts with, the server they
reach, the browsing screen over its library, and the audio a song picked in
it plays through.

This is also where every capability the player has is built, and the only
place that names the record holding them: everything under it says what it
touches with a @Has@ class, and so can touch nothing else.

A run is one account after another. Browsing an account ends either in the
player being left, which ends the run, or in a logout, which forgets that
account and asks the login screen for the next one — so a run browses as
many libraries as it is logged into, one at a time.
-}
module Havidrome
  ( run

    -- * Where a run starts
  , Start (..)
  , start
  , checked

    -- * One account after another
  , Account (..)
  , player
  ) where

import Data.Foldable (traverse_)
import Data.Text as T (Text)
import Havidrome.Audio (Audio, HasAudio (getAudio), withAudio)
import Havidrome.Browse.Screen (Ending (LoggedOut, Quit), browsing, opening)
import Havidrome.Clock (Clock, HasClock (getClock), mkClock)
import Havidrome.Credentials qualified as Credentials
import Havidrome.Credentials.Store
  ( HasStore (getStore)
  , Store (discard, load)
  , Stored (Absent, Present, Unreadable)
  , mkStore
  )
import Havidrome.Journal (HasJournal (getJournal), Journal, mkJournal)
import Havidrome.Library (Library (artists))
import Havidrome.Login.Screen qualified as Login
import Havidrome.Playback (newSession)
import Havidrome.Subsonic
  ( HasSubsonic (getSubsonic)
  , Subsonic (accepts, addresses, browses)
  , SubsonicError (AuthRejected)
  , explain
  , mkSubsonic
  )
import Havidrome.Terminal (HasTerminal (getTerminal), Terminal (says), mkTerminal)
import System.Exit (exitFailure)

{- | Every capability the player has, built by 'run' and named nowhere else.
A function that took this would claim the whole world, so none does: each
one asks for the capabilities it uses by constraint instead.
-}
data Env = Env
  { store :: Store
  , subsonic :: Subsonic
  , audio :: Audio
  , terminal :: Terminal
  , clock :: Clock
  , journal :: Journal
  }

instance HasStore Env where
  getStore env = env.store

instance HasSubsonic Env where
  getSubsonic env = env.subsonic

instance HasAudio Env where
  getAudio env = env.audio

instance HasTerminal Env where
  getTerminal env = env.terminal

instance HasClock Env where
  getClock env = env.clock

instance HasJournal Env where
  getJournal env = env.journal

{- | Where a run starts, once the config file has been read and, when it held
credentials, the server has been asked about them.
-}
data Start
  = {- | The login screen asks for an account, carrying what the server said
    about the stored credentials, when there were any and it said anything.
    -}
    Ask (Maybe Text)
  | -- | These are stored and the server takes them: browse, asking nothing.
    Browse Credentials.Credentials
  deriving stock (Eq, Show)

{- | What a run does with what the config file held: where it starts, or —
'Left' — the reason it cannot start at all, in the line it says before it
stops.

Credentials that are stored are not browsed on sight: 'checked' asks the
server about them first.
-}
start :: Stored -> Either Text Start
start = \case
  Absent -> Right (Ask Nothing)
  Present credentials -> Right (Browse credentials)
  Unreadable fault -> Left (Credentials.explain fault)

{- | What a run does with the server's answer about the credentials it found
stored.

A refusal is not the end of the run: it puts up the login screen with the
refusal on it, and another account can be entered there. Every other answer
is a server the run cannot browse, and it says why and stops. Nothing here
throws the refused credentials away — only a logout does that — so they
stay stored until ones a server takes replace them.
-}
checked :: Credentials.Credentials -> Either SubsonicError () -> Either Text Start
checked credentials = \case
  Right () -> Right (Browse credentials)
  Left refusal@(AuthRejected _) -> Right (Ask (Just (explain refusal)))
  Left failure -> Left (explain failure)

{- | Runs the player to completion: the library the credentials reach, browsed
until the user quits, and every library logged into after it.

Credentials that are already stored are put to the server they name before
anything is browsed with them; when there are none, or the server refuses
the ones there are, the login screen asks for them, and a run left at that
screen browses nothing at all.

Every capability is built here, once, and lasts exactly as long as the run:
the player mpv makes the sound with is started before the first screen and
gone after the last, and one account after another is played through it.
The journal is built before anything else, because the capabilities that
recover from an exception write to it and so are built on top of it: a
journal is the smallest environment that has one.
-}
run :: IO ()
run = do
  journal <- mkJournal
  subsonic <- mkSubsonic journal
  withAudio journal $ \audio -> do
    let env =
          Env
            { store = mkStore journal
            , subsonic
            , audio
            , terminal = mkTerminal
            , clock = mkClock
            , journal
            }
    started <- start <$> (getStore env).load
    opened <- case started of
      Right (Browse credentials) ->
        checked credentials <$> (getSubsonic env).accepts credentials
      settled -> pure settled
    either (stop env) (player (accounts env)) opened

{- | What a run does with an account: where it gets one, what browsing it comes
to, and how it is forgotten again.

The player's are the login screen, the browsing screen and the config file
('accounts'); a spec's stand-in is as good an account as far as 'player' is
concerned, which is what the @f@ keeps open.
-}
data Account f = Account
  { asks :: Maybe Text -> f (Maybe Credentials.Credentials)
  {- ^ Credentials a server took, or nothing when the player was left instead,
  asked for on a screen carrying what a server has already said.
  -}
  , browses :: Credentials.Credentials -> f Ending
  -- ^ Browse the library these credentials reach, until browsing ends.
  , forgets :: f ()
  -- ^ Throw away the stored credentials, so that a later run asks again.
  }

{- | The player's own: the login screen asks, the browsing screen browses, and
the config file is what forgetting empties.
-}
accounts
  :: (HasAudio env, HasClock env, HasStore env, HasSubsonic env, HasTerminal env)
  => env
  -> Account IO
accounts env =
  Account
    { asks = Login.login env (Login.navidrome env)
    , browses = browse env
    , forgets = (getStore env).discard
    }

{- | One account after another, from where the run starts: the credentials it
opens with, or the login screen asking for the first.

A logout forgets the account before the next is asked for, so that a run cut
short at that login screen leaves nothing stored behind it. Only a logout
forgets anything: a login screen a run opens on because its stored
credentials were refused leaves them exactly where they are. Leaving the
player, at the login screen or under it, ends the run there and then.
-}
player :: (Monad f) => Account f -> Start -> f ()
player account = \case
  Ask said -> asked said
  Browse credentials -> entered credentials
 where
  asked said = account.asks said >>= traverse_ entered
  entered credentials =
    account.browses credentials >>= \case
      Quit -> pure ()
      LoggedOut -> account.forgets >> asked Nothing

{- | Opens the artist list of the server these credentials reach, hands the
terminal over to it, and says how browsing it ended.

The session that plays what is picked is this account's own, so an account
logged out of leaves no album behind it, and the next one starts on nothing.
-}
browse
  :: (HasAudio env, HasClock env, HasSubsonic env, HasTerminal env)
  => env
  -> Credentials.Credentials
  -> IO Ending
browse env credentials = do
  let subsonic = getSubsonic env
      browsed = subsonic.browses credentials
  browsed.artists >>= \case
    Left failure -> stop env (explain failure)
    Right artists -> do
      session <- newSession (getAudio env) (subsonic.addresses credentials)
      browsing env browsed session (opening artists)

-- | Says why the player cannot go on, and stops.
stop :: (HasTerminal env) => env -> Text -> IO a
stop env reason = (getTerminal env).says ("havidrome: " <> reason) >> exitFailure
