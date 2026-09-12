-- | The journal the player writes its own record into: a file beside the
-- config one, holding a line for every exception the player caught and carried
-- on from.
--
-- It is @$XDG_STATE_HOME\/havidrome\/journal@, and the file is the user's own
-- to read, as the config file beside it is: a line there can carry the
-- server's address or the account's name, and nobody but its owner is let
-- near it.
--
-- Nothing on screen comes from here. The journal writes to that file and
-- nowhere else, so a run looks exactly as it did before there was one — and
-- that holds when the writing itself fails: a journal that cannot reach its
-- file gives up on the line rather than raising where the player was already
-- recovering from something.
module Havidrome.Journal
  ( Journal (..)
  , HasJournal (..)
  , mkJournal
  ) where

import Control.Exception (IOException, try)
import Data.ByteString qualified as ByteString
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory
  ( XdgDirectory (XdgState)
  , createDirectoryIfMissing
  , getXdgDirectory
  )
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

-- | Where the player's own record is kept, and how a line joins it.
data Journal = Journal
  { file :: IO FilePath
  -- ^ Where the lines are kept: @$XDG_STATE_HOME\/havidrome\/journal@, and
  -- @~\/.local\/state\/havidrome\/journal@ when @XDG_STATE_HOME@ is unset.
  , writes :: Text -> IO ()
  -- ^ Adds this line to what is already there, stamped with the moment it was
  -- written, creating the state directory and the file if they are missing. A
  -- line joins the ones before it: no line, and no run, empties the file.
  }

class HasJournal env where
  getJournal :: env -> Journal

-- | The journal itself is the smallest environment that has one. It is what
-- lets the handles built before the player's @Env@ exists — the config file,
-- the transport, the audio backend — ask for a journal by the same constraint
-- everything above them uses.
instance HasJournal Journal where
  getJournal = id

-- | The journal under the XDG state directory the run is given, opened with
-- the line that says a run has begun — so a run leaves a journal behind it
-- whether or not it had anything to recover from, and the lines of one run are
-- told from the next's by where those lines begin.
--
-- Nothing is held open afterwards: each line finds the file for itself.
mkJournal :: IO Journal
mkJournal = do
  writing "the player started"
  pure
    Journal
      { file = journalFile
      , writes = writing
      }

journalFile :: IO FilePath
journalFile = (</> "journal") <$> getXdgDirectory XdgState "havidrome"

-- | One line, appended. A line the file will not take is dropped: see the
-- note on the module.
writing :: Text -> IO ()
writing said = ignoringIO $ do
  path <- journalFile
  let directory = takeDirectory path
  createDirectoryIfMissing True directory
  setFileMode directory ownerOnlyDirectory
  at <- getCurrentTime
  let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" at)
  ByteString.appendFile path (encodeUtf8 (stamp <> " " <> oneLine said <> "\n"))
  setFileMode path ownerOnlyFile

-- | An entry is a line, so whatever an exception had to say about itself is
-- flattened onto one.
oneLine :: Text -> Text
oneLine = T.unwords . T.words

ignoringIO :: IO () -> IO ()
ignoringIO act = do
  attempt <- try act
  case attempt of
    Left (_ :: IOException) -> pure ()
    Right () -> pure ()

ownerOnlyFile :: FileMode
ownerOnlyFile = 0o600

ownerOnlyDirectory :: FileMode
ownerOnlyDirectory = 0o700
