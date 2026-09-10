{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The player itself: the credentials a run starts with, the server they
-- reach, the browsing screen over its library, and the audio a song picked in
-- it plays through.
module Havidrome
  ( run

    -- * Where a run starts
  , Start (..)
  , start
  ) where

import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.IO qualified as Text.IO
import Havidrome.Audio (withAudio)
import Havidrome.Browse.Screen (browsing, opening)
import Havidrome.Credentials (Stored (Absent, Present, Unreadable))
import Havidrome.Credentials qualified as Credentials
import Havidrome.Library qualified as Library
import Havidrome.Login qualified as Login
import Havidrome.Playback (newSession)
import Havidrome.Subsonic
  ( Credentials (Credentials)
  , Server (Server)
  , explain
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
    Stop ("the stored credentials could not be read: " <> Text.pack (show fault))

-- | Runs the player to completion: the library the credentials reach, browsed
-- until the user quits.
--
-- Credentials that are already stored are used as they are; when there are
-- none, the login screen asks for them, and a run left at that screen browses
-- nothing at all.
run :: IO ()
run =
  start <$> Credentials.load >>= \case
    Ask -> Login.login Login.navidrome >>= traverse_ browse
    Browse credentials -> browse credentials
    Stop reason -> stop reason

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
