-- | The player's whole conversation with a Navidrome server: check a set of
-- credentials, walk the library artist to album to song, and say where a
-- song's audio lives.
--
-- Every call is made as an account, and an account names its server as well
-- as its user, so one record serves every server a run talks to and holds no
-- default of its own: nothing here is tied to a particular server.
module Havidrome.Subsonic
  ( -- * The calls
    Subsonic (..)
  , HasSubsonic (..)
  , mkSubsonic
  , subsonicOver

    -- * Vocabulary
  , module Havidrome.Subsonic.Types
  , Transport (..)
  , Salt
  , mkSalt
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Generics (Generic)
import Havidrome.Credentials qualified as Credentials
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
import Havidrome.Subsonic.Transport (Transport (..), mkHttpTransport)
import Havidrome.Subsonic.Types
import Optics.Core (view, (%))
import System.Random (randomRIO)

-- | Everything the player asks a Navidrome server, whatever account it asks
-- as.
data Subsonic = Subsonic
  { accepts :: Credentials.Credentials -> IO (Either SubsonicError ())
  -- ^ Whether the server this account names accepts it. @Right ()@ is
  -- acceptance; a refusal and an unreachable server are both 'Left', and are
  -- told apart by which 'SubsonicError' it is.
  , browses :: Credentials.Credentials -> Library IO
  -- ^ The library that account can walk.
  , addresses :: Credentials.Credentials -> SongId -> Text
  -- ^ Where a song's audio is, for that account: a plain GET, asking for the
  -- file the server stores and never for a transcode of it.
  }

class HasSubsonic env where
  getSubsonic :: env -> Subsonic

-- | The calls over a transport of its own and a salt drawn for this run.
mkSubsonic :: IO Subsonic
mkSubsonic = subsonicOver <$> mkHttpTransport <*> randomSalt

-- | The calls over a transport and a salt the caller chooses.
subsonicOver :: Transport -> Salt -> Subsonic
subsonicOver transport salt =
  Subsonic
    { accepts = ping . client
    , browses = libraryOf . client
    , addresses = audioFor . client
    }
  where
    client = clientFor transport salt

-- | A server, the account presented to it, the salt that account's tokens are
-- signed with, and the way out to the network.
data Client = Client
  { server :: Server
  , credentials :: Credentials
  , salt :: Salt
  , transport :: Transport
  }
  deriving stock (Generic)

clientFor :: Transport -> Salt -> Credentials.Credentials -> Client
clientFor transport salt account =
  Client
    { server = Server account.server
    , credentials = Credentials account.username account.password
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

ping :: Client -> IO (Either SubsonicError ())
ping client = call client Ping decodePing

-- | The library a Navidrome server holds. Every call can fail, and hands back
-- the 'SubsonicError' it failed with in place of its list, never a half-list.
libraryOf :: Client -> Library IO
libraryOf client =
  Library
    { artists = fmap byArtistName <$> call client GetArtists decodeArtists
    , albums = \artist ->
        fmap byAlbumYear <$> call client (GetArtist artist) decodeAlbums
    , songs = \album ->
        fmap byTrackOrder <$> call client (GetAlbum album) decodeSongs
    }

audioFor :: Client -> SongId -> Text
audioFor client = audioUrl client.server client.credentials client.salt

-- | Draws a fresh salt. Sixteen characters from an alphabet that needs no
-- escaping in a URL, comfortably over the six the API asks for.
randomSalt :: IO Salt
randomSalt = mkSalt . Text.pack <$> traverse (const draw) [1 :: Int .. 16]
  where
    alphabet = "abcdefghijklmnopqrstuvwxyz0123456789"
    draw = (alphabet !!) <$> randomRIO (0, length alphabet - 1)
