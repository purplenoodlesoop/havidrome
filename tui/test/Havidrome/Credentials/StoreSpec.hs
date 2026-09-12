-- | The credential store, exercised against a throwaway config directory so
-- that nothing here can touch the real one.
module Havidrome.Credentials.StoreSpec (spec) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.Text as T (Text)
import Data.Text.IO qualified as T.IO
import Havidrome.Credentials (Credentials (..), Fault (..))
import Havidrome.Credentials.Store (Store (..), Stored (..), mkStore)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)

spec :: Spec
spec = do
  whereItLives
  saving
  loading
  discarding

whereItLives :: Spec
whereItLives = describe "the config file" $ do
  it "sits under XDG_CONFIG_HOME when it is set" $
    withConfigHome $ \home ->
      store.file `shouldReturn` home </> "havidrome" </> "config"

  it "falls back to ~/.config when XDG_CONFIG_HOME is unset" $
    withSystemTempDirectory "havidrome-home" $ \home ->
      withEnvironment "XDG_CONFIG_HOME" Nothing $
        withEnvironment "HOME" (Just home) $ do
          store.save account
          store.file
            `shouldReturn` home </> ".config" </> "havidrome" </> "config"
          doesFileExist (home </> ".config" </> "havidrome" </> "config")
            `shouldReturn` True

saving :: Spec
saving = describe "save" $ do
  it "creates the config directory when it is missing" $
    withSystemTempDirectory "havidrome-config" $ \parent ->
      withEnvironment "XDG_CONFIG_HOME" (Just (parent </> "fresh")) $ do
        store.save account
        path <- store.file
        doesFileExist path `shouldReturn` True

  it "keeps the file to the user who owns it" $
    withConfigHome $ \_ -> do
      store.save account
      path <- store.file
      status <- getFileStatus path
      fileMode status .&. 0o777 `shouldBe` 0o600

  it "replaces credentials saved before" $
    withConfigHome $ \_ -> do
      store.save account
      store.save other
      store.load `shouldReturn` Present other

loading :: Spec
loading = describe "load" $ do
  it "returns exactly what was saved" $
    withConfigHome $ \_ -> do
      store.save account
      store.load `shouldReturn` Present account

  it "returns a password of punctuation and non-ASCII characters unchanged" $
    withConfigHome $ \_ -> do
      let awkward = account {password = "pä ss=wörd\\ 密码 \"#!\" \t "}
      store.save awkward
      store.load `shouldReturn` Present awkward

  it "returns a password of backslashes and newlines unchanged" $
    withConfigHome $ \_ -> do
      let awkward = account {password = "one\\ntwo\nthree\\\n"}
      store.save awkward
      store.load `shouldReturn` Present awkward

  it "reports an escape the store never writes" $
    withConfigHome $ \_ -> do
      writeConfig "server=a\\qb\nusername=someone\npassword=hunter2\n"
      store.load `shouldReturn` Unreadable (BadLine "server=a\\qb")

  it "reports no stored credentials when no file exists" $
    withConfigHome $ \_ ->
      store.load `shouldReturn` Absent

  it "reports a file that is not credentials at all" $
    withConfigHome $ \_ -> do
      writeConfig "just some prose\n"
      store.load `shouldReturn` Unreadable (BadLine "just some prose")

  it "reports a field the file never sets" $
    withConfigHome $ \_ -> do
      writeConfig "server=https://music.example.com\nusername=someone\n"
      store.load `shouldReturn` Unreadable (MissingField "password")

  it "reports a field the file sets twice" $
    withConfigHome $ \_ -> do
      writeConfig
        "server=https://music.example.com\nusername=someone\npassword=a\npassword=b\n"
      store.load `shouldReturn` Unreadable (RepeatedField "password")

  it "reports a file that is not UTF-8" $
    withConfigHome $ \_ -> do
      path <- store.file
      createDirectoryIfMissing True (takeDirectory path)
      ByteString.writeFile path (ByteString.pack [0x73, 0x3d, 0xff, 0xfe])
      stored <- store.load
      stored `shouldSatisfy` \case
        Unreadable (NotAccessible _) -> True
        _ -> False

discarding :: Spec
discarding = describe "discard" $ do
  it "leaves no stored credentials behind" $
    withConfigHome $ \_ -> do
      store.save account
      store.discard
      store.load `shouldReturn` Absent

  it "does nothing when nothing is stored" $
    withConfigHome $ \_ -> do
      store.discard
      store.load `shouldReturn` Absent

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

-- | Runs an action with a config directory of its own, thrown away after.
withConfigHome :: (FilePath -> IO a) -> IO a
withConfigHome use =
  withSystemTempDirectory "havidrome-config" $ \home ->
    withEnvironment "XDG_CONFIG_HOME" (Just home) (use home)

-- | Runs an action with one environment variable set, or unset, restoring
-- whatever it held before.
withEnvironment :: FilePath -> Maybe FilePath -> IO a -> IO a
withEnvironment name value action =
  bracket (lookupEnv name <* apply value) apply (const action)
  where
    apply = maybe (unsetEnv name) (setEnv name)

-- | Puts contents in the config file that no save would write.
writeConfig :: Text -> IO ()
writeConfig contents = do
  path <- store.file
  createDirectoryIfMissing True (takeDirectory path)
  T.IO.writeFile path contents
