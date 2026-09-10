{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The player itself: the stored credentials, the server they reach, and the
-- browsing screen over its library.
module Havidrome (run) where

import Brick (defaultMain)
import Control.Monad (void)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Havidrome.Browse.Screen (application, explain, opening)
import Havidrome.Credentials (Stored (Absent, Present, Unreadable))
import Havidrome.Credentials qualified as Credentials
import Havidrome.Library qualified as Library
import Havidrome.Subsonic (Credentials (Credentials), Server (Server), newClient)
import System.Exit (exitFailure)
import System.IO (stderr)

-- | Runs the player to completion: the library the stored credentials reach,
-- browsed until the user quits.
--
-- Asking for credentials that are not stored yet is the login screen's job and
-- is not built; until it is, a run with nothing stored says so and stops.
run :: IO ()
run =
  Credentials.load >>= \case
    Absent ->
      stop "no credentials are stored, and the screen that asks for them is not built yet"
    Unreadable fault ->
      stop ("the stored credentials could not be read: " <> Text.pack (show fault))
    Present credentials -> browse credentials

-- | Opens the artist list of the server these credentials reach, and hands the
-- terminal over to it.
browse :: Credentials.Credentials -> IO ()
browse credentials = do
  client <-
    newClient
      (Server (Credentials.server credentials))
      (Credentials (Credentials.username credentials) (Credentials.password credentials))
  let library = Library.subsonic client
  runExceptT (Library.artists library) >>= \case
    Left failure -> stop (explain failure)
    Right artists -> void (defaultMain (application library) (opening artists))

-- | Says why the player cannot go on, and stops.
stop :: Text -> IO ()
stop reason = Text.IO.hPutStrLn stderr ("havidrome: " <> reason) >> exitFailure
