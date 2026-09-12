{- | The Subsonic wire protocol, as pure functions: the URL of every call the
player makes, the reading of every answer, and the orders the lists are
shown in. Nothing here performs I\/O, so all of it is testable without a
server.
-}
module Havidrome.Subsonic.Protocol
  ( -- * Authentication
    Salt
  , mkSalt

    -- * Requests
  , Endpoint (..)
  , endpointUrl
  , audioUrl

    -- * Responses
  , decodePing
  , decodeArtists
  , decodeAlbums
  , decodeSongs

    -- * Orders
  , byArtistName
  , byAlbumYear
  , byTrackOrder
  ) where

import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (Object, (.!=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither, withObject)
import Data.ByteString (ByteString)
import Data.List (sortOn)
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Encoding qualified as T
import Havidrome.Subsonic.Types
import Network.HTTP.Types.URI (renderSimpleQuery)

{- | The Subsonic API version the player speaks. Navidrome answers anything up
to its own; 1.16.1 is the last one, and covers every call made here.
-}
apiVersion :: ByteString
apiVersion = "1.16.1"

-- | The name Subsonic servers log this client under.
clientName :: ByteString
clientName = "havidrome"

{- | The random string mixed into the password before it is hashed, so the
password itself never travels. One salt serves a whole session: the salt
exists to keep the hash unguessable, not to make each request unique.
-}
newtype Salt = Salt Text
  deriving stock (Eq, Show)

-- | Takes a salt as given, for callers that need a predictable one.
mkSalt :: Text -> Salt
mkSalt = Salt

{- | @md5(password <> salt)@, hex-encoded, which is what the server compares
against.
-}
token :: Credentials -> Salt -> ByteString
token credentials (Salt salt) =
  T.encodeUtf8 . T.pack . show $
    (hash (encodeUtf8 (credentials.password <> salt)) :: Digest MD5)

{- | The parameters every call carries: who is asking, the proof, and what is
asking.
-}
authQuery :: Credentials -> Salt -> [(ByteString, ByteString)]
authQuery credentials salt@(Salt s) =
  [ ("u", encodeUtf8 credentials.user)
  , ("t", token credentials salt)
  , ("s", encodeUtf8 s)
  , ("v", apiVersion)
  , ("c", clientName)
  ]

{- | The calls the player makes. Every one of them is a list the browsing
screens show, except the ping that checks a set of credentials.
-}
data Endpoint
  = -- | Checks credentials against a server.
    Ping
  | -- | Every artist in the library.
    GetArtists
  | -- | One artist's albums.
    GetArtist ArtistId
  | -- | One album's songs.
    GetAlbum AlbumId
  deriving stock (Eq, Show)

endpointName :: Endpoint -> Text
endpointName endpoint = case endpoint of
  Ping -> "ping"
  GetArtists -> "getArtists"
  GetArtist _ -> "getArtist"
  GetAlbum _ -> "getAlbum"

endpointQuery :: Endpoint -> [(ByteString, ByteString)]
endpointQuery endpoint = case endpoint of
  Ping -> []
  GetArtists -> []
  GetArtist (ArtistId artist) -> [("id", encodeUtf8 artist)]
  GetAlbum (AlbumId album) -> [("id", encodeUtf8 album)]

-- | Where to GET a call's JSON answer.
endpointUrl :: Server -> Credentials -> Salt -> Endpoint -> Text
endpointUrl server credentials salt endpoint =
  restUrl server (endpointName endpoint) $
    endpointQuery endpoint <> [("f", "json")] <> authQuery credentials salt

{- | Where to GET a song's audio. @format=raw@ is Subsonic's "send the file you
have"; no other format is ever named, so the server transcodes nothing and
the player receives the stored original, whatever it is.
-}
audioUrl :: Server -> Credentials -> Salt -> SongId -> Text
audioUrl server credentials salt (SongId song) =
  restUrl server "stream" $
    [("id", encodeUtf8 song), ("format", "raw")] <> authQuery credentials salt

restUrl :: Server -> Text -> [(ByteString, ByteString)] -> Text
restUrl server name query =
  T.dropWhileEnd (== '/') server.url
    <> "/rest/"
    <> name
    <> T.decodeUtf8 (renderSimpleQuery True query)

{- | Unwraps a @subsonic-response@ and reads the payload out of it, turning
every way that can go wrong into a 'SubsonicError'.
-}
decodeEnvelope :: (Object -> Parser a) -> ByteString -> Either SubsonicError a
decodeEnvelope payload body = case Aeson.eitherDecodeStrict' body of
  Left message -> malformed message
  Right value -> case parseEither envelope value of
    Left message -> malformed message
    Right result -> result
 where
  malformed = Left . MalformedResponse . T.pack

  envelope = withObject "Subsonic response" $ \outer -> do
    response <- outer .: "subsonic-response"
    status <- response .: "status"
    if status == ("ok" :: Text)
      then Right <$> payload response
      else do
        reported <- traverse apiError =<< response .:? "error"
        pure (Left (failureFrom reported))

  apiError = withObject "Subsonic error" $ \o ->
    (,) <$> o .: "code" <*> o .:? "message" .!= "the server gave no reason"

{- | Classifies a server's own failure. The codes that mean "these credentials
are no good" are kept apart from the rest, because the player answers them
differently.
-}
failureFrom :: Maybe (Int, Text) -> SubsonicError
failureFrom reported = case reported of
  Nothing -> ServerFailure 0 "the server reported a failure without saying why"
  Just (code, message)
    | code `elem` credentialCodes -> AuthRejected message
    | otherwise -> ServerFailure code message
 where
  -- 40 wrong username or password, 41 token authentication not supported,
  -- 42 authentication mechanism not supported, 43 conflicting mechanisms,
  -- 44 invalid API key.
  credentialCodes = [40, 41, 42, 43, 44]

-- | A ping carries no payload: reaching @ok@ is the whole answer.
decodePing :: ByteString -> Either SubsonicError ()
decodePing = decodeEnvelope (const (pure ()))

{- | @getArtists@ groups artists under index letters; the grouping is the
server's, and the player shows one flat list.
-}
decodeArtists :: ByteString -> Either SubsonicError [Artist]
decodeArtists = decodeEnvelope $ \response -> do
  artists <- response .: "artists"
  indexes <- artists .:? "index" .!= []
  concat <$> traverse (\index -> traverse artist =<< index .:? "artist" .!= []) indexes
 where
  artist = withObject "artist" $ \o ->
    Artist . ArtistId <$> o .: "id" <*> o .: "name"

-- | The albums @getArtist@ reports for the artist that was asked for.
decodeAlbums :: ByteString -> Either SubsonicError [Album]
decodeAlbums = decodeEnvelope $ \response -> do
  artist <- response .: "artist"
  traverse album =<< artist .:? "album" .!= []
 where
  album = withObject "album" $ \o ->
    Album . AlbumId <$> o .: "id" <*> o .: "name" <*> o .:? "year"

{- | The songs @getAlbum@ reports for the album that was asked for. A song the
server gives no duration for is taken as zero rather than rejected, so one
odd track cannot cost the caller the whole album.
-}
decodeSongs :: ByteString -> Either SubsonicError [Song]
decodeSongs = decodeEnvelope $ \response -> do
  album <- response .: "album"
  traverse song =<< album .:? "song" .!= []
 where
  song = withObject "song" $ \o ->
    Song
      . SongId
      <$> o .: "id"
      <*> o .: "title"
      <*> (Seconds <$> o .:? "duration" .!= 0)
      <*> o .:? "track"
      <*> o .:? "discNumber"

{- | Alphabetically, ignoring case; exact name then id settle the ties, so the
list is the same on every run.
-}
byArtistName :: [Artist] -> [Artist]
byArtistName = sortOn (\a -> (T.toCaseFold a.name, a.name, a.id))

-- | Oldest year first; name then id settle the ties.
byAlbumYear :: [Album] -> [Album]
byAlbumYear = sortOn (\a -> (a.year, T.toCaseFold a.name, a.id))

{- | Album order: disc, then track within the disc; title then id settle the
ties.
-}
byTrackOrder :: [Song] -> [Song]
byTrackOrder = sortOn (\s -> (s.disc, s.track, T.toCaseFold s.title, s.id))
