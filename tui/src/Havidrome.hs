-- | The player itself: the credentials a run starts with, the server they
-- reach, the browsing screen over its library, and the audio a song picked in
-- it plays through.
--
-- A run is one account after another. Browsing an account ends either in the
-- player being left, which ends the run, or in a logout, which forgets that
-- account and asks the login screen for the next one — so a run browses as
-- many libraries as it is logged into, one at a time.
module Havidrome
  ( run

    -- * Where a run starts
  , Start (..)
  , start

    -- * One account after another
  , Account (..)
  , player
  ) where

import Data.Functor.Compose (getCompose)
import Data.Foldable (traverse_)
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as T.IO
import Havidrome.Audio (withAudio)
import Havidrome.Browse.Screen (Ending (LoggedOut, Quit), browsing, opening)
import Havidrome.Credentials qualified as Credentials
import Havidrome.Credentials.Store (Stored (Absent, Present, Unreadable))
import Havidrome.Credentials.Store qualified as Store
import Havidrome.Library (Library (artists))
import Havidrome.Login.Screen qualified as Login
import Havidrome.Playback (newSession)
import Havidrome.Subsonic
  ( Credentials (Credentials)
  , Server (Server)
  , explain
  , library
  , newClient
  , songAudioUrl
  )
import System.Exit (exitFailure)
import System.IO (stderr)

-- | Where a run starts, which is settled by what the config file holds.
data Start
  = -- | Nothing is stored: the login screen asks for it.
    Ask
  | -- | These are stored: browse the server they reach, asking nothing.
    Browse Credentials.Credentials
  | -- | Something is stored that is not credentials, and the run cannot go on.
    Stop Text
  deriving stock (Eq, Show)

-- | What a run does with what the config file held.
start :: Stored -> Start
start = \case
  Absent -> Ask
  Present credentials -> Browse credentials
  Unreadable fault ->
    Stop ("the stored credentials could not be read: " <> T.pack (show fault))

-- | Runs the player to completion: the library the credentials reach, browsed
-- until the user quits, and every library logged into after it.
--
-- Credentials that are already stored are used as they are; when there are
-- none, the login screen asks for them, and a run left at that screen browses
-- nothing at all.
run :: IO ()
run = do
  loaded <- Store.load
  case start loaded of
    Ask -> player havidrome Nothing
    Browse credentials -> player havidrome (Just credentials)
    Stop reason -> stop reason

-- | What a run does with an account: where it gets one, what browsing it comes
-- to, and how it is forgotten again.
--
-- The player's are the login screen, the browsing screen and the config file
-- ('havidrome'); a spec's stand-in is as good an account as far as 'player' is
-- concerned, which is what the @f@ keeps open.
data Account f = Account
  { asks :: f (Maybe Credentials.Credentials)
  -- ^ Credentials a server took, or nothing when the player was left instead.
  , browses :: Credentials.Credentials -> f Ending
  -- ^ Browse the library these credentials reach, until browsing ends.
  , forgets :: f ()
  -- ^ Throw away the stored credentials, so that a later run asks again.
  }

-- | The player's own: the login screen asks, the browsing screen browses, and
-- the config file is what forgetting empties.
havidrome :: Account IO
havidrome =
  Account
    { asks = Login.login Login.navidrome
    , browses = browse
    , forgets = Store.discard
    }

-- | One account after another, from the credentials a run starts with — or
-- from none, which is the login screen asking for the first.
--
-- A logout forgets the account before the next is asked for, so that a run cut
-- short at that login screen leaves nothing stored behind it. Leaving the
-- player, at the login screen or under it, ends the run there and then.
player :: (Monad f) => Account f -> Maybe Credentials.Credentials -> f ()
player account = maybe asked entered
  where
    asked = account.asks >>= traverse_ entered
    entered credentials =
      account.browses credentials >>= \case
        Quit -> pure ()
        LoggedOut -> account.forgets >> asked

-- | Opens the artist list of the server these credentials reach, hands the
-- terminal over to it, and says how browsing it ended.
--
-- The audio backend is started only once there is a library to browse, and is
-- gone again when browsing ends, so a run that never reaches a list never
-- reaches for a player either, and an account logged out of takes its backend
-- with it.
browse :: Credentials.Credentials -> IO Ending
browse credentials = do
  client <-
    newClient
      (Server credentials.server)
      (Credentials credentials.username credentials.password)
  let browsed = library client
  getCompose browsed.artists >>= \case
    Left failure -> stop (explain failure)
    Right artists -> withAudio $ \audio -> do
      session <- newSession audio (songAudioUrl client)
      browsing browsed session (opening artists)

-- | Says why the player cannot go on, and stops.
stop :: Text -> IO a
stop reason = T.IO.hPutStrLn stderr ("havidrome: " <> reason) >> exitFailure
