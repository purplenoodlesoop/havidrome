-- | The credential store, exercised against a throwaway config directory so
-- that nothing here can touch the real one.
module CredentialsSpec (spec) where

import Control.Exception (bracket)
import Data.Bits ((.&.))
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Havidrome.Credentials (Credentials (..), Fault (..), Stored (..))
import Havidrome.Credentials qualified as Credentials
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.FilePath (takeDirectory, (</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Posix.Files (fileMode, getFileStatus)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck ((===))

spec :: Spec
spec = describe "Havidrome.Credentials" $ do
  describe "configFile" $ do
    it "sits under XDG_CONFIG_HOME when it is set" $
      withConfigHome $ \home ->
        Credentials.configFile `shouldReturn` home </> "havidrome" </> "config"

    it "falls back to ~/.config when XDG_CONFIG_HOME is unset" $
      withSystemTempDirectory "havidrome-home" $ \home ->
        withEnvironment "XDG_CONFIG_HOME" Nothing $
          withEnvironment "HOME" (Just home) $ do
            Credentials.save account
            Credentials.configFile
              `shouldReturn` home </> ".config" </> "havidrome" </> "config"
            doesFileExist (home </> ".config" </> "havidrome" </> "config")
              `shouldReturn` True

  describe "save" $ do
    it "creates the config directory when it is missing" $
      withSystemTempDirectory "havidrome-config" $ \parent ->
        withEnvironment "XDG_CONFIG_HOME" (Just (parent </> "fresh")) $ do
          Credentials.save account
          path <- Credentials.configFile
          doesFileExist path `shouldReturn` True

    it "keeps the file to the user who owns it" $
      withConfigHome $ \_ -> do
        Credentials.save account
        path <- Credentials.configFile
        status <- getFileStatus path
        (fileMode status .&. 0o777) `shouldBe` 0o600

    it "replaces credentials saved before" $
      withConfigHome $ \_ -> do
        Credentials.save account
        Credentials.save other
        Credentials.load `shouldReturn` Present other

  describe "load" $ do
    it "returns exactly what was saved" $
      withConfigHome $ \_ -> do
        Credentials.save account
        Credentials.load `shouldReturn` Present account

    it "returns a password of punctuation and non-ASCII characters unchanged" $
      withConfigHome $ \_ -> do
        let awkward = account {password = "pä ss=wörd\\ 密码 \"#!\" \t "}
        Credentials.save awkward
        Credentials.load `shouldReturn` Present awkward

    it "returns a password of backslashes and newlines unchanged" $
      withConfigHome $ \_ -> do
        let awkward = account {password = "one\\ntwo\nthree\\\n"}
        Credentials.save awkward
        Credentials.load `shouldReturn` Present awkward

    it "reports an escape the store never writes" $
      withConfigHome $ \_ -> do
        writeConfig "server=a\\qb\nusername=someone\npassword=hunter2\n"
        Credentials.load `shouldReturn` Unreadable (BadLine "server=a\\qb")

    it "reports no stored credentials when no file exists" $
      withConfigHome $ \_ ->
        Credentials.load `shouldReturn` Absent

    it "reports a file that is not credentials at all" $
      withConfigHome $ \_ -> do
        writeConfig "just some prose\n"
        Credentials.load `shouldReturn` Unreadable (BadLine "just some prose")

    it "reports a field the file never sets" $
      withConfigHome $ \_ -> do
        writeConfig "server=https://music.example.com\nusername=someone\n"
        Credentials.load `shouldReturn` Unreadable (MissingField "password")

    it "reports a field the file sets twice" $
      withConfigHome $ \_ -> do
        writeConfig
          "server=https://music.example.com\nusername=someone\npassword=a\npassword=b\n"
        Credentials.load `shouldReturn` Unreadable (RepeatedField "password")

    it "reports a file that is not UTF-8" $
      withConfigHome $ \_ -> do
        path <- Credentials.configFile
        createDirectoryIfMissing True (takeDirectory path)
        ByteString.writeFile path (ByteString.pack [0x73, 0x3d, 0xff, 0xfe])
        stored <- Credentials.load
        stored `shouldSatisfy` \case
          Unreadable (NotAccessible _) -> True
          _ -> False

  describe "discard" $ do
    it "leaves no stored credentials behind" $
      withConfigHome $ \_ -> do
        Credentials.save account
        Credentials.discard
        Credentials.load `shouldReturn` Absent

    it "does nothing when nothing is stored" $
      withConfigHome $ \_ -> do
        Credentials.discard
        Credentials.load `shouldReturn` Absent

  describe "render and parse" $ do
    it "writes one field per line, in the clear" $
      Credentials.render account
        `shouldBe` "server=https://music.example.com\nusername=someone\npassword=hunter2\n"

    prop "read back whatever was written, whatever it holds" $
      \(server', username', password') ->
        let credentials =
              Credentials
                { server = Text.pack server'
                , username = Text.pack username'
                , password = Text.pack password'
                }
         in Credentials.parse (Credentials.render credentials) === Right credentials

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
withEnvironment :: String -> Maybe String -> IO a -> IO a
withEnvironment name value action =
  bracket (lookupEnv name <* apply value) apply (const action)
  where
    apply = maybe (unsetEnv name) (setEnv name)

-- | Puts contents in the config file that no 'Credentials.save' would write.
writeConfig :: Text -> IO ()
writeConfig contents = do
  path <- Credentials.configFile
  createDirectoryIfMissing True (takeDirectory path)
  Text.IO.writeFile path contents
