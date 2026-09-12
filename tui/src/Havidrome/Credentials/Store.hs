-- | The config file the credentials live in between runs: where it is, and
-- reading, writing and throwing it away.
--
-- It is @$XDG_CONFIG_HOME/havidrome/config@, and what stands in it is the
-- shape 'Havidrome.Credentials' settles. Nothing here fails with an exception
-- a caller has to catch: a file that is not credentials comes back as a
-- 'Havidrome.Credentials.Fault'.
module Havidrome.Credentials.Store
  ( -- * The config file
    Store (..)
  , HasStore (..)
  , mkStore

    -- * What a load found
  , Stored (..)
  ) where

import Control.Exception (IOException, displayException, try)
import Control.Monad (when)
import Data.ByteString qualified as ByteString
import Data.Text qualified as Text
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Havidrome.Credentials (Credentials, Fault (NotAccessible), parse, render)
import System.Directory
  ( XdgDirectory (XdgConfig)
  , createDirectoryIfMissing
  , doesFileExist
  , getXdgDirectory
  , removeFile
  )
import System.FilePath (takeDirectory, (</>))
import System.Posix.Files (setFileMode)
import System.Posix.Types (FileMode)

-- | The one file the player keeps, and everything it does with it.
data Store = Store
  { file :: IO FilePath
  -- ^ Where the credentials live: @$XDG_CONFIG_HOME/havidrome/config@, and
  -- @~\/.config\/havidrome\/config@ when @XDG_CONFIG_HOME@ is unset.
  , load :: IO Stored
  -- ^ Reads the stored credentials, if there are any to read.
  , save :: Credentials -> IO ()
  -- ^ Stores a set of credentials, replacing whatever was stored before and
  -- creating the config directory if it is missing. The file is the user's
  -- own: it holds a password in the clear, so nobody else is let near it.
  , discard :: IO ()
  -- ^ Throws away the stored credentials, leaving nothing behind for a later
  -- load to find. Discarding when nothing is stored does nothing.
  }

class HasStore env where
  getStore :: env -> Store

-- | What a load found in the config file.
data Stored
  = -- | Nothing is stored. Not an error: it is what a first run finds.
    Absent
  | -- | Something is stored, but it is not credentials.
    Unreadable Fault
  | -- | The stored credentials.
    Present Credentials
  deriving stock (Eq, Show)

-- | The config file under the XDG directory the run is given. It opens
-- nothing and holds nothing open, so it is not in 'IO'; each operation finds
-- the file for itself.
mkStore :: Store
mkStore =
  Store
    { file = configFile
    , load = loading
    , save = saving
    , discard = discarding
    }

configFile :: IO FilePath
configFile = (</> "config") <$> getXdgDirectory XdgConfig "havidrome"

loading :: IO Stored
loading = do
  path <- configFile
  exists <- doesFileExist path
  if not exists
    then pure Absent
    else either unreachable readable <$> try (ByteString.readFile path)
  where
    unreachable :: IOException -> Stored
    unreachable = Unreadable . NotAccessible . Text.pack . displayException

    readable bytes = case decodeUtf8' bytes of
      Left _ -> Unreadable (NotAccessible "the file is not valid UTF-8")
      Right text -> either Unreadable Present (parse text)

saving :: Credentials -> IO ()
saving credentials = do
  path <- configFile
  let directory = takeDirectory path
  createDirectoryIfMissing True directory
  setFileMode directory ownerOnlyDirectory
  ByteString.writeFile path (encodeUtf8 (render credentials))
  setFileMode path ownerOnlyFile

discarding :: IO ()
discarding = do
  path <- configFile
  exists <- doesFileExist path
  when exists (removeFile path)

ownerOnlyFile :: FileMode
ownerOnlyFile = 0o600

ownerOnlyDirectory :: FileMode
ownerOnlyDirectory = 0o700
