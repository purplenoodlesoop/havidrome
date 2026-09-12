-- | The credential store, exercised against a throwaway config directory so
-- that nothing here can touch the real one.
module Havidrome.Credentials.StoreTest (tests) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.Foldable (traverse_)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.IO qualified as Text.IO
import Havidrome.Check (example)
import Havidrome.Credentials (Credentials (..), Fault (BadLine, MissingField, NotAccessible, RepeatedField))
import Havidrome.Credentials.Store (Store (..), Stored (Absent, Present, Unreadable), mkStore)
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
    ,
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
    ,
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
    ,
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
    ,
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
    ,
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

-- | The store as the player builds it. It finds the config file for itself,
-- so this one value serves every throwaway directory below.
store :: Store
store = mkStore

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
  address = "abcdefghijklmnopqrstuvwxyz.:/-0123456789" :: String
  awkward = "\\\n=\t \"" :: String

-- | Runs an action with a config directory of its own, thrown away after.
withConfigHome :: (FilePath -> IO a) -> IO a
withConfigHome use =
  withSystemTempDirectory "havidrome-config" $ \home ->
    withEnvironment "XDG_CONFIG_HOME" (Just home) (use home)

-- | Runs an action with one environment variable set, or unset, restoring
-- whatever it held before.
withEnvironment :: String -> Maybe String -> IO a -> IO a
withEnvironment name value action =
  bracket (lookupEnv name <* apply value) apply (const action)
 where
  apply = maybe (unsetEnv name) (setEnv name)

-- | What the store makes of a config file holding exactly this, which no save
-- would have written.
loading :: Text -> IO Stored
loading contents = withConfigHome $ \_ -> do
  path <- store.file
  createDirectoryIfMissing True (takeDirectory path)
  Text.IO.writeFile path contents
  store.load
