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

    -- * Vocabulary
  , module Havidrome.Subsonic.Types
  , Transport (..)
  , Salt
  , mkSalt
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
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
  , randomSalt
  )
import Havidrome.Subsonic.Transport (Transport (..), newHttpTransport)
import Havidrome.Subsonic.Types

-- | A server, the credentials to present to it, and the way out to the
-- network.
data Client = Client
  { clientServer :: Server
  , clientCredentials :: Credentials
  , clientSalt :: Salt
  , clientTransport :: Transport
  }

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
    { clientServer = server
    , clientCredentials = credentials
    , clientSalt = salt
    , clientTransport = transport
    }

call ::
  Client ->
  Endpoint ->
  (ByteString -> Either SubsonicError a) ->
  IO (Either SubsonicError a)
call client endpoint decode = do
  answer <-
    fetch
      (clientTransport client)
      (endpointUrl (clientServer client) (clientCredentials client) (clientSalt client) endpoint)
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
  audioUrl (clientServer client) (clientCredentials client) (clientSalt client)
