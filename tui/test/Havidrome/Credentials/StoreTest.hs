-- | The credential store, exercised against a throwaway config directory so
-- that nothing here can touch the real one.
module Havidrome.Credentials.StoreTest (tests) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import Data.ByteString qualified as ByteString
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as Text.IO
import Havidrome.Check (Checks, example)
import Havidrome.Credentials (Credentials (..), Fault (BadLine, MissingField, NotAccessible, RepeatedField))
import Havidrome.Credentials.Store (Store (..), Stored (Absent, Present, Unreadable), mkStore)
import Havidrome.Journal.Fake (silent)
import Hedgehog (Gen, Group (Group), assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)

tests :: Group
tests =
  Group
    "Havidrome.Credentials.Store"
    ( location
        <> saving
        <> loaded
        <> faults
        <> discarding
        <> anySave
    )

-- | Where the config file is, given what the environment says.
location :: Checks
location =
  [
    ( "the config file sits under XDG_CONFIG_HOME when it is set"
    , example do
        (home, path) <- evalIO (withConfigHome (\home -> (home,) <$> store.file))
        path === home </> "havidrome" </> "config"
    )
  ,
    ( "the config file falls back to ~/.config when XDG_CONFIG_HOME is unset"
    , example do
        (wanted, path, written) <- evalIO . withSystemTempDirectory "havidrome-home" $ \home -> do
          let wanted = home </> ".config" </> "havidrome" </> "config"
          withEnvironment "XDG_CONFIG_HOME" Nothing . withEnvironment "HOME" (Just home) $ do
            store.save account
            path <- store.file
            written <- doesFileExist wanted
            pure (wanted, path, written)
        path === wanted
        assert written
    )
  ]

-- | What a save leaves on disk.
saving :: Checks
saving =
  [
    ( "save creates the config directory when it is missing"
    , example do
        written <- evalIO . withSystemTempDirectory "havidrome-config" $ \parent ->
          withEnvironment "XDG_CONFIG_HOME" (Just (parent </> "fresh")) $ do
            store.save account
            store.file >>= doesFileExist
        assert written
    )
  ,
    ( "save keeps the file to the user who owns it"
    , example do
        mode <- evalIO . withConfigHome $ \_ -> do
          store.save account
          path <- store.file
          status <- getFileStatus path
          pure (fileMode status .&. 0o777)
        mode === 0o600
    )
  ,
    ( "save replaces credentials saved before"
    , example do
        stored <- evalIO . withConfigHome $ \_ -> do
          store.save account
          store.save other
          store.load
        stored === Present other
    )
  ]

-- | What a load gives back of what was saved.
loaded :: Checks
loaded =
  [
    ( "load returns exactly what was saved"
    , example do
        stored <- evalIO (withConfigHome (\_ -> store.save account >> store.load))
        stored === Present account
    )
  ,
    ( "load returns a password of punctuation and non-ASCII characters unchanged"
    , example do
        let awkward = account {password = "pä ss=wörd\\ 密码 \"#!\" \t "}
        stored <- evalIO (withConfigHome (\_ -> store.save awkward >> store.load))
        stored === Present awkward
    )
  ,
    ( "load returns a password of backslashes and newlines unchanged"
    , example do
        let awkward = account {password = "one\\ntwo\nthree\\\n"}
        stored <- evalIO (withConfigHome (\_ -> store.save awkward >> store.load))
        stored === Present awkward
    )
  ,
    ( "load reports an escape the store never writes"
    , example do
        stored <- evalIO (loading "server=a\\qb\nusername=someone\npassword=hunter2\n")
        stored === Unreadable (BadLine "server=a\\qb")
    )
  ]

-- | What a load makes of a file no save would have written.
faults :: Checks
faults =
  [
    ( "load reports no stored credentials when no file exists"
    , example do
        stored <- evalIO (withConfigHome (const store.load))
        stored === Absent
    )
  ,
    ( "load reports a file that is not credentials at all"
    , example do
        stored <- evalIO (loading "just some prose\n")
        stored === Unreadable (BadLine "just some prose")
    )
  ,
    ( "load reports a field the file never sets"
    , example do
        stored <- evalIO (loading "server=https://music.example.com\nusername=someone\n")
        stored === Unreadable (MissingField "password")
    )
  ,
    ( "load reports a field the file sets twice"
    , example do
        stored <-
          evalIO . loading $
            "server=https://music.example.com\nusername=someone\npassword=a\npassword=b\n"
        stored === Unreadable (RepeatedField "password")
    )
  ,
    ( "load reports a file that is not UTF-8"
    , example do
        stored <- evalIO . withConfigHome $ \_ -> do
          path <- store.file
          createDirectoryIfMissing True (takeDirectory path)
          ByteString.writeFile path (ByteString.pack [0x73, 0x3d, 0xff, 0xfe])
          store.load
        assert (case stored of Unreadable (NotAccessible _) -> True; _ -> False)
    )
  ]

-- | What a discard leaves behind.
discarding :: Checks
discarding =
  [
    ( "discard leaves no stored credentials behind"
    , example do
        stored <- evalIO . withConfigHome $ \_ -> do
          store.save account
          store.discard
          store.load
        stored === Absent
    )
  ,
    ( "discard does nothing when nothing is stored"
    , example do
        stored <- evalIO (withConfigHome (\_ -> store.discard >> store.load))
        stored === Absent
    )
  ]

-- | What holds of credentials of any shape, saved any number of times.
anySave :: Checks
anySave =
  [
    ( "load returns whatever credentials were saved, however they are written"
    , property do
        them <- forAll anAccount
        stored <- evalIO (withConfigHome (\_ -> store.save them >> store.load))
        stored === Present them
    )
  ,
    ( "the last save is the one that stands, however many came before it"
    , property do
        earlier <- forAll (Gen.list (Range.linear 0 4) anAccount)
        latest <- forAll anAccount
        stored <- evalIO . withConfigHome $ \_ -> do
          traverse_ store.save earlier
          store.save latest
          store.load
        stored === Present latest
    )
  ,
    ( "a saved file is the user's own, whatever it holds"
    , property do
        them <- forAll anAccount
        mode <- evalIO . withConfigHome $ \_ -> do
          store.save them
          path <- store.file
          (.&. 0o777) . fileMode <$> getFileStatus path
        mode === 0o600
    )
  ,
    ( "a discard after any number of saves leaves nothing to find"
    , property do
        saved <- forAll (Gen.list (Range.linear 0 4) anAccount)
        stored <- evalIO . withConfigHome $ \_ -> do
          traverse_ store.save saved
          store.discard
          store.load
        stored === Absent
    )
  ]

-- | The store as the player builds it, over a journal that keeps nothing. It
-- finds the config file for itself, so this one value serves every throwaway
-- directory below.
store :: Store
store = mkStore silent

account :: Credentials
account =
  Credentials
    { server = "https://music.example.com"
    , username = "someone"
    , password = "hunter2"
    }

other :: Credentials
other =
  Credentials
    { server = "https://other.example.com"
    , username = "someone-else"
    , password = "correct horse"
    }

-- | Credentials of every shape a user can type: any characters at all in the
-- password, the ones the file has to escape among them.
anAccount :: Gen Credentials
anAccount = do
  server <- Gen.text (Range.linear 1 40) (Gen.element address)
  username <- Gen.text (Range.linear 0 20) Gen.unicode
  password <- Gen.text (Range.linear 0 40) (Gen.frequency [(3, Gen.unicode), (1, Gen.element awkward)])
  pure Credentials {server = "https://" <> server, username, password}
 where
  address = T.unpack "abcdefghijklmnopqrstuvwxyz.:/-0123456789"
  awkward = T.unpack "\\\n=\t \""

-- | Runs an action with a config directory of its own, thrown away after.
withConfigHome :: (FilePath -> IO a) -> IO a
withConfigHome use =
  withSystemTempDirectory "havidrome-config" $ \home ->
    withEnvironment "XDG_CONFIG_HOME" (Just home) (use home)

-- | Runs an action with one environment variable set, or unset, restoring
-- whatever it held before.
withEnvironment :: Text -> Maybe FilePath -> IO a -> IO a
withEnvironment name value action =
  bracket (lookupEnv named <* apply value) apply (const action)
 where
  named = T.unpack name
  apply = maybe (unsetEnv named) (setEnv named)

-- | What the store makes of a config file holding exactly this, which no save
-- would have written.
loading :: Text -> IO Stored
loading contents = withConfigHome $ \_ -> do
  path <- store.file
  createDirectoryIfMissing True (takeDirectory path)
  Text.IO.writeFile path contents
  store.load
