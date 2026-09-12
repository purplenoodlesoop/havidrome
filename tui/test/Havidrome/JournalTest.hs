-- | The journal: the file it writes to, what a run leaves in it, and the line
-- a caught exception puts there.
--
-- Everything here runs against a throwaway state directory, so that nothing in
-- the suite touches the journal of whoever is running it.
module Havidrome.JournalTest (tests) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.IORef (readIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Havidrome.Check (example)
import Havidrome.Journal (Journal (file, writes), mkJournal)
import Havidrome.Journal.Fake (recording)
import Havidrome.Subsonic.Transport (Transport (fetch), mkHttpTransport)
import Hedgehog (Group (Group), Property, assert, evalIO, (===))
import System.Directory (doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)

tests :: Group
tests =
  Group
    "Havidrome.Journal"
    [ ("the file sits under XDG_STATE_HOME when it is set", underStateHome)
    , ("the file falls back to ~/.local/state when that is unset", underHome)
    , ("a run leaves a journal behind, state directory and all", leavesAFile)
    , ("a later run's lines follow the earlier run's, emptying nothing", appends)
    , ("the file is its owner's to read and write, and nobody else's", ownerOnly)
    , ("a caught exception writes a line where it was caught", caughtIsJournalled)
    ]

underStateHome :: Property
underStateHome = example do
  (found, home) <- evalIO . withStateHome $ \home -> do
    journal <- mkJournal
    (,home) <$> journal.file
  found === home </> "havidrome" </> "journal"

underHome :: Property
underHome = example do
  (found, home) <- evalIO . withSystemTempDirectory "havidrome-home" $ \home ->
    withEnvironment "XDG_STATE_HOME" Nothing
      . withEnvironment "HOME" (Just home)
      $ do
        journal <- mkJournal
        (,home) <$> journal.file
  found === home </> ".local" </> "state" </> "havidrome" </> "journal"

-- | Opening a journal is the whole of what a run has to do to leave one: the
-- state directory need not have been there, and nothing need have gone wrong.
leavesAFile :: Property
leavesAFile = example do
  there <- evalIO . withSystemTempDirectory "havidrome-state" $ \parent ->
    withEnvironment "XDG_STATE_HOME" (Just (parent </> "fresh")) $ do
      journal <- mkJournal
      journal.file >>= doesFileExist
  assert there

-- | Each run builds a journal of its own, and each one finds the same file and
-- adds to what is already in it.
appends :: Property
appends = example do
  said <- evalIO . withStateHome . const $ do
    firstRun <- mkJournal
    firstRun.writes "the first run said this"
    firstRun.writes "and then this"
    secondRun <- mkJournal
    secondRun.writes "the second run said this"
    secondRun.file >>= Text.IO.readFile
  fmap unstamped (Text.lines said)
    === [ "the player started"
        , "the first run said this"
        , "and then this"
        , "the player started"
        , "the second run said this"
        ]

ownerOnly :: Property
ownerOnly = example do
  mode <- evalIO . withStateHome . const $ do
    journal <- mkJournal
    journal.writes "something happened"
    journal.file >>= fmap fileMode . getFileStatus
  mode .&. 0o777 === 0o600

-- | The transport catches everything a request can suffer, and an address
-- nothing answers on is the plainest of them. The journal it is handed is a
-- value of the same record type as the player's, keeping its lines where a
-- test can read them back instead of putting them in a file.
caughtIsJournalled :: Property
caughtIsJournalled = example do
  (failed, written) <- evalIO $ do
    (journal, recorded) <- recording
    transport <- mkHttpTransport journal
    answer <- transport.fetch nowhere
    (either (const True) (const False) answer,) <$> readIORef recorded
  assert failed
  fmap (Text.isPrefixOf ("the request to " <> nowhere <> " failed: ")) written === [True]

-- | A line without the moment it was stamped with, which is whatever the clock
-- said and so is not a thing to assert on.
unstamped :: Text -> Text
unstamped = Text.unwords . drop 1 . Text.words

-- | An invented address nothing answers on, so that the request fails the way
-- it fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://nowhere.example/rest/ping"

-- | An empty state directory of its own, named to the action.
withStateHome :: (FilePath -> IO a) -> IO a
withStateHome use =
  withSystemTempDirectory "havidrome-state" $ \home ->
    withEnvironment "XDG_STATE_HOME" (Just home) (use home)

-- | Runs an action with a variable set, or unset, putting back whatever was
-- there before.
withEnvironment :: String -> Maybe String -> IO a -> IO a
withEnvironment name value action =
  bracket (lookupEnv name) (restore name) (const (restore name value >> action))

restore :: String -> Maybe String -> IO ()
restore name = maybe (unsetEnv name) (setEnv name)
