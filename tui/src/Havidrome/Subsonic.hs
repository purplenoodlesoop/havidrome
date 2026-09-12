-- | The player's whole conversation with a Navidrome server: check a set of
-- credentials, walk the library artist to album to song, and say where a
-- song's audio lives.
--
-- A 'Client' is pointed at a server by its caller and holds no default of its
-- own, so nothing here is tied to a particular server.
module Havidrome.Subsonic
  ( -- * The client
    Client
  , newClient
  , clientOver

    -- * Calls
  , checkCredentials
  , listArtists
  , listAlbums
  , listSongs
  , songAudioUrl

    -- * The library browsing walks
  , library

    -- * Vocabulary
  , module Havidrome.Subsonic.Types
  , Transport (..)
  , Salt
  , mkSalt
  ) where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Havidrome.Library (Library (..))
import Havidrome.Subsonic.Protocol
  ( Endpoint (..)
  , Salt
  , audioUrl
  , byAlbumYear
  , byArtistName
  , byTrackOrder
  , decodeAlbums
  , decodeArtists
  , decodePing
  , decodeSongs
  , endpointUrl
  , mkSalt
  )
import Havidrome.Subsonic.Transport (Transport (..), newHttpTransport)
import Havidrome.Subsonic.Types
import Optics.Core (view, (%))
import System.Random (randomRIO)

-- | A server, the credentials to present to it, and the way out to the
-- network.
data Client = Client
  { server :: Server
  , credentials :: Credentials
  , salt :: Salt
  , transport :: Transport
  }
  deriving stock (Generic)

-- | A client that speaks HTTP, with a salt drawn for this session.
newClient :: Server -> Credentials -> IO Client
newClient server credentials = do
  transport <- newHttpTransport
  salt <- randomSalt
  pure (clientOver transport salt server credentials)

-- | A client over a transport and a salt the caller chooses.
clientOver :: Transport -> Salt -> Server -> Credentials -> Client
clientOver transport salt server credentials =
  Client
    { server
    , credentials
    , salt
    , transport
    }

call ::
  Client ->
  Endpoint ->
  (ByteString -> Either SubsonicError a) ->
  IO (Either SubsonicError a)
call client endpoint decode = do
  answer <-
    view (#transport % #fetch) client
      (endpointUrl client.server client.credentials client.salt endpoint)
  pure (answer >>= decode)

-- | Whether the server accepts the client's credentials. @Right ()@ is
-- acceptance; a refusal and an unreachable server are both 'Left', and are
-- told apart by which 'SubsonicError' it is.
checkCredentials :: Client -> IO (Either SubsonicError ())
checkCredentials client = call client Ping decodePing

-- | Every artist in the library, alphabetically.
listArtists :: Client -> IO (Either SubsonicError [Artist])
listArtists client = fmap (fmap byArtistName) (call client GetArtists decodeArtists)

-- | One artist's albums, oldest year first.
listAlbums :: Client -> ArtistId -> IO (Either SubsonicError [Album])
listAlbums client artist = fmap (fmap byAlbumYear) (call client (GetArtist artist) decodeAlbums)

-- | One album's songs, in album order.
listSongs :: Client -> AlbumId -> IO (Either SubsonicError [Song])
listSongs client album = fmap (fmap byTrackOrder) (call client (GetAlbum album) decodeSongs)

-- | Where a song's audio is: a plain GET, asking for the file the server
-- stores and never for a transcode of it.
songAudioUrl :: Client -> SongId -> Text
songAudioUrl client =
  audioUrl client.server client.credentials client.salt

-- | The library a Navidrome server holds. Every call can fail, and a failure
-- stops the fetch it was part of rather than yielding a half-list, which is
-- what the 'ExceptT' says.
library :: Client -> Library (ExceptT SubsonicError IO)
library client =
  Library
    { artists = ExceptT (listArtists client)
    , albums = ExceptT . listAlbums client
    , songs = ExceptT . listSongs client
    }

-- | Draws a fresh salt. Sixteen characters from an alphabet that needs no
-- escaping in a URL, comfortably over the six the API asks for.
randomSalt :: IO Salt
randomSalt = mkSalt . Text.pack <$> traverse (const draw) [1 :: Int .. 16]
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
    draw = (alphabet !!) <$> randomRIO (0, length alphabet - 1)
