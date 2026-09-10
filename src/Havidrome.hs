{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The player itself: the stored credentials, the server they reach, the
-- browsing screen over its library, and the audio a song picked in it plays
-- through.
module Havidrome (run) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Havidrome.Audio (withAudio)
import Havidrome.Browse.Screen (browsing, explain, opening)
import Havidrome.Credentials (Stored (Absent, Present, Unreadable))
import Havidrome.Credentials qualified as Credentials
import Havidrome.Library qualified as Library
import Havidrome.Playback (newSession)
import Havidrome.Subsonic (Credentials (Credentials), Server (Server), newClient, songAudioUrl)
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
--
-- The audio backend is started only once there is a library to browse, and is
-- gone again when browsing ends, so a run that never reaches a list never
-- reaches for a player either.
browse :: Credentials.Credentials -> IO ()
browse credentials = do
  client <-
    newClient
      (Server (Credentials.server credentials))
      (Credentials (Credentials.username credentials) (Credentials.password credentials))
  let library = Library.subsonic client
  runExceptT (Library.artists library) >>= \case
    Left failure -> stop (explain failure)
    Right artists -> withAudio $ \audio -> do
      session <- newSession audio (songAudioUrl client)
      browsing library session (opening artists)

-- | Says why the player cannot go on, and stops.
stop :: Text -> IO ()
stop reason = Text.IO.hPutStrLn stderr ("havidrome: " <> reason) >> exitFailure
