-- | The vocabulary of the Subsonic client: what it is pointed at, what it
-- hands back, and the ways it can fail.
module Havidrome.Subsonic.Types
  ( -- * Where to talk, and as whom
    Server (..)
  , Credentials (..)

    -- * The library
  , ArtistId (..)
  , AlbumId (..)
  , SongId (..)
  , Artist (..)
  , Album (..)
  , Song (..)
  , Seconds (..)

    -- * Failure
  , SubsonicError (..)
  , explain
  ) where

import Data.Text as T (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- | The base address of a Navidrome server, as typed at the login screen —
-- @https:\/\/music.example.org@, with or without a trailing slash. Nothing is
-- hardcoded to a particular server.
newtype Server = Server {url :: Text}
  deriving stock (Eq, Show)

-- | A username and password to present to a server.
data Credentials = Credentials
  { user :: Text
  , password :: Text
  }
  deriving stock (Eq)

-- | Shows the user but never the password, so credentials cannot leak into a
-- log line or an error message by accident.
instance Show Credentials where
  showsPrec d credentials =
    showParen (d > 10) $
      showString "Credentials {user = "
        . shows credentials.user
        . showString ", password = <hidden>}"

newtype ArtistId = ArtistId Text
  deriving stock (Eq, Ord, Show)

newtype AlbumId = AlbumId Text
  deriving stock (Eq, Ord, Show)

newtype SongId = SongId Text
  deriving stock (Eq, Ord, Show)

-- | A whole number of seconds.
newtype Seconds = Seconds Int
  deriving stock (Eq, Ord, Show)

data Artist = Artist
  { id :: ArtistId
  , name :: Text
  }
  deriving stock (Eq, Show)

data Album = Album
  { id :: AlbumId
  , name :: Text
  , -- | Absent when the server records no year for the album.
    year :: Maybe Int
  }
  deriving stock (Eq, Show)

data Song = Song
  { id :: SongId
  , -- | The track name the now-playing overlay shows.
    title :: Text
  , -- | The track's total time, the other half of the overlay.
    duration :: Seconds
  , -- | Absent when the server records no track number.
    track :: Maybe Int
  , -- | Absent on single-disc albums, and when the server records no disc.
    disc :: Maybe Int
  }
  deriving stock (Eq, Generic, Show)

-- | Why a call did not produce an answer. The player responds differently to
-- each: a network failure and rejected credentials both keep the login screen
-- up with the error, while the others mean the server answered something we
-- could not use.
data SubsonicError
  = -- | The server could not be reached at all: unknown host, refused
    -- connection, TLS failure, timeout, or an address that is not a usable
    -- URL.
    NetworkFailure Text
  | -- | The server was reached and refused these credentials.
    AuthRejected Text
  | -- | The server was reached and answered with a failure of its own,
    -- carrying the Subsonic error code and its message.
    ServerFailure Int Text
  | -- | The answer was not a Subsonic response we understand.
    MalformedResponse Text
  deriving stock (Eq, Show)

-- | What went wrong, in a sentence for the strip along the bottom of whatever
-- screen is up.
explain :: SubsonicError -> Text
explain = \case
  NetworkFailure reason -> "The server could not be reached: " <> reason
  AuthRejected reason -> "The server refused these credentials: " <> reason
  ServerFailure code reason ->
    "The server answered with an error (" <> T.pack (show code) <> "): " <> reason
  MalformedResponse reason -> "The server's answer could not be read: " <> reason
