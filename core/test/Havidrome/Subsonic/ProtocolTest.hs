-- | The calls the client makes and the answers it reads back: the URL each
-- one is asked at, what the payload becomes, and the orders the lists are put
-- into.
module Havidrome.Subsonic.ProtocolTest (tests) where

import Data.Aeson (Value, object, (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Key (Key)
import Data.Aeson.Types (Pair)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LazyByteString
import Data.List (sort, sortOn)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
import Havidrome.Subsonic.Fixtures
  ( albumsAnswer
  , albumsWithoutYearAnswer
  , artistsAnswer
  , emptyAlbumAnswer
  , notFoundAnswer
  , pingAnswer
  , songWithoutDurationAnswer
  , songsAnswer
  , songsWithoutTrackAnswer
  , testCredentials
  , testSalt
  , testServer
  , testToken
  , unexplainedFailureAnswer
  , wrongPasswordAnswer
  )
import Havidrome.Subsonic.Protocol
  ( Endpoint (GetAlbum, GetArtist, GetArtists, Ping)
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
import Havidrome.Subsonic.Types
  ( Album (..)
  , AlbumId (AlbumId)
  , Artist (..)
  , ArtistId (ArtistId)
  , Credentials (Credentials, password)
  , Seconds (Seconds)
  , Server (Server)
  , Song (..)
  , SongId (SongId)
  , SubsonicError (AuthRejected, MalformedResponse, ServerFailure)
  )
import Hedgehog (Gen, Group (Group), assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Subsonic.Protocol"
    ( signing
        <> addressing
        <> serverAddress
        <> audio
        <> reading
        <> readingSongs
        <> refusing
        <> artistOrder
        <> albumOrder
        <> songOrder
    )

-- | How a call is signed, and what it never carries.
signing :: Checks
signing =
  [
    ( "a ping is signed with the salted password hash, never the password"
    , example (url Ping === "https://music.example.org/rest/ping?f=json&" <> credentialQuery)
    )
  ,
    ( "a request never carries the password itself"
    , example (assert (not (testCredentials.password `T.isInfixOf` url Ping)))
    )
  ,
    ( "no request carries the password, whoever is asking and whatever is asked for"
    , property do
        server <- forAll anyServer
        credentials <- forAll anyCredentials
        salt <- forAll anySalt
        endpoint <- forAll anyEndpoint
        song <- forAll (SongId <$> anyId)
        assert
          (not (credentials.password `T.isInfixOf` endpointUrl server credentials salt endpoint))
        assert
          (not (credentials.password `T.isInfixOf` audioUrl server credentials salt song))
    )
  ]

-- | The address each call is made at.
addressing :: Checks
addressing =
  [
    ( "one artist's albums are asked for by id"
    , example
        ( url (GetArtist (ArtistId "a1"))
            === "https://music.example.org/rest/getArtist?id=a1&f=json&" <> credentialQuery
        )
    )
  ,
    ( "one album's songs are asked for by id"
    , example
        ( url (GetAlbum (AlbumId "b1"))
            === "https://music.example.org/rest/getAlbum?id=b1&f=json&" <> credentialQuery
        )
    )
  ,
    ( "a call about one artist, album or song carries that id, whichever it is"
    , property do
        server <- forAll anyServer
        credentials <- forAll anyCredentials
        salt <- forAll anySalt
        ident <- forAll anyId
        endpoint <- forAll (Gen.element [GetArtist (ArtistId ident), GetAlbum (AlbumId ident)])
        assert
          (("id=" <> ident <> "&") `T.isInfixOf` endpointUrl server credentials salt endpoint)
        assert
          ( ("id=" <> ident <> "&")
              `T.isInfixOf` audioUrl server credentials salt (SongId ident)
          )
    )
  ]

-- | Where a request is addressed, whatever the server's own address ends in.
serverAddress :: Checks
serverAddress =
  [
    ( "a slash the server address already ends with is not doubled"
    , example
        ( endpointUrl (Server "https://music.example.org/") testCredentials testSalt Ping
            === url Ping
        )
    )
  ,
    ( "a request is addressed under the server's own address, however it ends"
    , property do
        Server address <- forAll anyServer
        slashes <- forAll (Gen.int (Range.linear 0 3))
        credentials <- forAll anyCredentials
        salt <- forAll anySalt
        endpoint <- forAll anyEndpoint
        let request = endpointUrl (Server address) credentials salt endpoint
        endpointUrl (Server (address <> T.replicate slashes "/")) credentials salt endpoint
          === request
        assert ((address <> "/rest/") `T.isPrefixOf` request)
    )
  ,
    ( "what a query string cannot carry literally is escaped"
    , example
        ( assert
            ( "u=some%20one%26x&"
                `T.isInfixOf` endpointUrl testServer (Credentials "some one&x" "hunter2") testSalt Ping
            )
        )
    )
  ]

-- | Asking for a song's audio, as the server stores it.
audio :: Checks
audio =
  [
    ( "audio is asked for as the file the server stores"
    , example
        ( audioUrl testServer testCredentials testSalt (SongId "s1")
            === "https://music.example.org/rest/stream?id=s1&format=raw&" <> credentialQuery
        )
    )
  ,
    ( "an audio request names no format other than the stored one, and no bit rate"
    , example do
        let request = audioUrl testServer testCredentials testSalt (SongId "s1")
        length (T.breakOnAll "format=" request) === 1
        assert ("format=raw" `T.isInfixOf` request)
        assert (not ("maxBitRate" `T.isInfixOf` request))
    )
  ]
-- | A ping, the artists and the albums, as the server answers them.
-- | What the server's answers are read back as.
reading :: Checks
reading =
  [
    ( "a ping is read"
    , example (decodePing pingAnswer === Right ())
    )
  ,
    ( "artists are read out of the server's index groups"
    , example
        ( fmap (fmap (.name)) (decodeArtists artistsAnswer)
            === Right ["zebra", "Aphex Twin", "anohni"]
        )
    )
  ,
    ( "every artist the server lists is read back, in its order, however it grouped them"
    , property do
        groups <- forAll (Gen.list (Range.linear 0 4) (Gen.list (Range.linear 0 5) anyArtist))
        decodeArtists (artistsAnswerOf groups) === Right (concat groups)
    )
  ,
    ( "albums are read, with the year the server gave or none"
    , example
        ( fmap (fmap (.year)) (decodeAlbums albumsAnswer)
            === Right [Just 2001, Just 1992, Nothing]
        )
    )
  ,
    ( "every album the server lists is read back, in its order, year or no year"
    , property do
        albums <- forAll (Gen.list (Range.linear 0 8) anyAlbum)
        decodeAlbums (albumsAnswerOf albums) === Right albums
    )
  ]

-- | The songs of an album, as the server lists them.
readingSongs :: Checks
readingSongs =
  [
    ( "songs are read with their track name and total time"
    , example
        ( fmap (fmap (\s -> (s.title, s.duration))) (decodeSongs songsAnswer)
            === Right
              [ ("Pulsewidth", Seconds 228)
              , ("Xtal", Seconds 293)
              , ("Tha", Seconds 543)
              ]
        )
    )
  ,
    ( "a song the server timed at nothing is given a total time of zero"
    , example
        ( fmap (fmap (.duration)) (decodeSongs songWithoutDurationAnswer)
            === Right [Seconds 0]
        )
    )
  ,
    ( "every song the server lists is read back, in its order, whatever it left out"
    , property do
        listed <- forAll (Gen.list (Range.linear 0 8) anyListedSong)
        decodeSongs (songsAnswerOf (fmap snd listed)) === Right (fmap fst listed)
    )
  ,
    ( "an album the server lists no songs for is read as empty"
    , example (decodeSongs emptyAlbumAnswer === Right [])
    )
  ]

-- | The answers that are no answer, and what they are told apart as.
refusing :: Checks
refusing =
  [
    ( "an answer that is not JSON is refused"
    , example (assert (isMalformed (decodePing "<html>gateway</html>")))
    )
  ,
    ( "an answer missing the payload the call asked for is refused"
    , example (assert (isMalformed (decodeArtists pingAnswer)))
    )
  ,
    ( "a rejected password is told from anything else"
    , example
        (decodePing wrongPasswordAnswer === Left (AuthRejected "Wrong username or password"))
    )
  ,
    ( "a server's other complaints are kept apart from rejected credentials"
    , example (decodeSongs notFoundAnswer === Left (ServerFailure 70 "Album not found"))
    )
  ,
    ( "the codes that mean the credentials were refused are told from every other code"
    , property do
        code <- forAll (Gen.int (Range.linear 0 100))
        reason <- forAll anyName
        decodePing (failedAnswerOf code reason)
          === Left
            ( if code `elem` [40, 41, 42, 43, 44]
                then AuthRejected reason
                else ServerFailure code reason
            )
    )
  ,
    ( "a failure the server gives no reason for is survived"
    , example
        ( assert case decodePing unexplainedFailureAnswer of
            Left (ServerFailure _ _) -> True
            _ -> False
        )
    )
  ]

-- | The order the artists come out in.
artistOrder :: Checks
artistOrder =
  [
    ( "artists are put in alphabetical order, whatever their capitals"
    , example
        ( fmap (fmap (.name) . byArtistName) (decodeArtists artistsAnswer)
            === Right ["anohni", "Aphex Twin", "zebra"]
        )
    )
  ,
    ( "any artists at all come out alphabetically, whatever their capitals"
    , property do
        artists <- forAll (Gen.list (Range.linear 0 12) anyArtist)
        assert (inOrder (fmap (T.toCaseFold . (.name)) (byArtistName artists)))
    )
  ,
    ( "ordering the artists loses none of them and invents none"
    , property do
        artists <- forAll (Gen.list (Range.linear 0 12) anyArtist)
        sortOn artistKey (byArtistName artists) === sortOn artistKey artists
    )
  ,
    ( "the artists come out the same however the server listed them"
    , property do
        artists <- forAll (Gen.list (Range.linear 0 12) anyArtist)
        byArtistName (reverse artists) === byArtistName artists
    )
  ]

-- | The order an artist's albums come out in.
albumOrder :: Checks
albumOrder =
  [
    ( "an artist's albums are put oldest year first"
    , example
        ( fmap (fmap (.name) . byAlbumYear) (decodeAlbums albumsAnswer)
            === Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]
        )
    )
  ,
    ( "an album the server gave no year for is put above the oldest, by name"
    , example
        ( fmap (fmap (.name) . byAlbumYear) (decodeAlbums albumsWithoutYearAnswer)
            === Right ["Demos", "Tapes", "Live"]
        )
    )
  ,
    ( "any albums at all come out oldest first, the ones with no year above them, by name"
    , property do
        albums <- forAll (Gen.list (Range.linear 0 12) anyAlbum)
        assert
          (inOrder (fmap (\a -> (a.year, T.toCaseFold a.name)) (byAlbumYear albums)))
    )
  ,
    ( "ordering the albums loses none of them and invents none"
    , property do
        albums <- forAll (Gen.list (Range.linear 0 12) anyAlbum)
        sortOn albumKey (byAlbumYear albums) === sortOn albumKey albums
    )
  ,
    ( "the albums come out the same however the server listed them"
    , property do
        albums <- forAll (Gen.list (Range.linear 0 12) anyAlbum)
        byAlbumYear (reverse albums) === byAlbumYear albums
    )
  ]

-- | The order an album's songs come out in.
songOrder :: Checks
songOrder =
  [
    ( "an album's songs are put in album order, disc by disc"
    , example
        ( fmap (fmap (.title) . byTrackOrder) (decodeSongs songsAnswer)
            === Right ["Xtal", "Tha", "Pulsewidth"]
        )
    )
  ,
    ( "a song the server gave no track number for is put first, by name"
    , example
        ( fmap (fmap (.title) . byTrackOrder) (decodeSongs songsWithoutTrackAnswer)
            === Right ["Loose end", "Sketch", "Opener"]
        )
    )
  ,
    ( "any songs at all come out disc by disc and track by track, the unnumbered above them, by name"
    , property do
        songs <- forAll (Gen.list (Range.linear 0 12) anySong)
        assert
          ( inOrder
              (fmap (\s -> (s.disc, s.track, T.toCaseFold s.title)) (byTrackOrder songs))
          )
    )
  ,
    ( "ordering the songs loses none of them and invents none"
    , property do
        songs <- forAll (Gen.list (Range.linear 0 12) anySong)
        sortOn songKey (byTrackOrder songs) === sortOn songKey songs
    )
  ,
    ( "the songs come out the same however the server listed them"
    , property do
        songs <- forAll (Gen.list (Range.linear 0 12) anySong)
        byTrackOrder (reverse songs) === byTrackOrder songs
    )
  ]

url :: Endpoint -> Text
url = endpointUrl testServer testCredentials testSalt

credentialQuery :: Text
credentialQuery = "u=someone&t=" <> testToken <> "&s=abc123&v=1.16.1&c=havidrome"

isMalformed :: Either SubsonicError a -> Bool
isMalformed answer = case answer of
  Left (MalformedResponse _) -> True
  _ -> False

-- | Whether a list is in the order it promises to come out in: nothing before
-- what should precede it.
inOrder :: (Ord a) => [a] -> Bool
inOrder xs = xs == sort xs

-- | Everything one of them is, so that two with the same key are the same
-- one: what tells a reordering that loses or invents nothing from one that
-- does.
artistKey :: Artist -> (ArtistId, Text)
artistKey artist = (artist.id, artist.name)

albumKey :: Album -> (AlbumId, Text, Maybe Int)
albumKey album = (album.id, album.name, album.year)

songKey :: Song -> (SongId, Text, Seconds, Maybe Int, Maybe Int)
songKey song = (song.id, song.title, song.duration, song.track, song.disc)

-- | An id as a server writes them: what a query string carries as it stands,
-- so a request can be searched for it without escaping it first.
anyId :: Gen Text
anyId = Gen.text (Range.linear 1 8) Gen.alphaNum

-- | A name for anything the library holds, capitals and scripts mixed, so an
-- order that ignores case has something to ignore.
anyName :: Gen Text
anyName = Gen.text (Range.linear 1 12) Gen.unicode

anyServer :: Gen Server
anyServer = do
  host <- Gen.text (Range.linear 1 10) Gen.alphaNum
  pure (Server ("https://" <> host <> ".example.org"))

-- | Someone to ask as. The password is longer than anything else a request
-- carries and begins outside the hex a hash is written in, so it cannot turn
-- up inside a URL by coincidence: finding it there means the request carried
-- it.
anyCredentials :: Gen Credentials
anyCredentials = do
  user <- Gen.text (Range.linear 1 10) Gen.alphaNum
  password <- Gen.text (Range.singleton 16) Gen.alphaNum
  pure (Credentials user ("pw" <> password))

anySalt :: Gen Salt
anySalt = mkSalt <$> Gen.text (Range.linear 1 8) Gen.alphaNum

anyEndpoint :: Gen Endpoint
anyEndpoint =
  Gen.choice
    [ pure Ping
    , pure GetArtists
    , GetArtist . ArtistId <$> anyId
    , GetAlbum . AlbumId <$> anyId
    ]

anyArtist :: Gen Artist
anyArtist = Artist . ArtistId <$> anyId <*> anyName

anyAlbum :: Gen Album
anyAlbum =
  Album . AlbumId
    <$> anyId
    <*> anyName
    <*> Gen.maybe (Gen.int (Range.linear 1900 2030))

-- | A song as a server lists it: the object it arrives in, beside the song it
-- must be read back as. Everything but the id and the title may be missing,
-- and a song the server does not time runs for no time at all.
anyListedSong :: Gen (Song, Value)
anyListedSong = do
  ident <- anyId
  title <- anyName
  duration <- Gen.maybe (Gen.int (Range.linear 0 6000))
  track <- Gen.maybe (Gen.int (Range.linear 1 30))
  disc <- Gen.maybe (Gen.int (Range.linear 1 4))
  pure
    ( Song
        { id = SongId ident
        , title
        , duration = maybe (Seconds 0) Seconds duration
        , track
        , disc
        }
    , object
        ( ["id" .= ident, "title" .= title]
            <> given "duration" duration
            <> given "track" track
            <> given "discNumber" disc
        )
    )

anySong :: Gen Song
anySong = fst <$> anyListedSong

-- | The text a library id is written as, which is all the server ever sees
-- of one.
artistIdText :: ArtistId -> Text
artistIdText (ArtistId ident) = ident

albumIdText :: AlbumId -> Text
albumIdText (AlbumId ident) = ident

-- | A field the server writes only when it has one.
given :: (Aeson.ToJSON a) => Key -> Maybe a -> [Pair]
given name = foldMap (\value -> [name .= value])

-- | Artists as @getArtists@ sends them: grouped under index letters, which is
-- the server's grouping and none of the player's business.
artistsAnswerOf :: [[Artist]] -> ByteString
artistsAnswerOf groups = okAnswer ["artists" .= object ["index" .= fmap index groups]]
  where
    index group =
      object ["name" .= ("X" :: Text), "artist" .= fmap artistJson group]

    artistJson artist =
      object ["id" .= artistIdText artist.id, "name" .= artist.name]

-- | Albums as @getArtist@ sends them, under the artist they belong to.
albumsAnswerOf :: [Album] -> ByteString
albumsAnswerOf albums =
  okAnswer
    [ "artist"
        .= object ["id" .= ("a1" :: Text), "name" .= ("Someone" :: Text), "album" .= fmap albumJson albums]
    ]
  where
    albumJson album =
      object
        ( ["id" .= albumIdText album.id, "name" .= album.name]
            <> given "year" album.year
        )

-- | Songs as @getAlbum@ sends them, under the album they belong to.
songsAnswerOf :: [Value] -> ByteString
songsAnswerOf songs =
  okAnswer
    ["album" .= object ["id" .= ("b1" :: Text), "name" .= ("Something" :: Text), "song" .= songs]]

-- | A successful answer carrying this payload.
okAnswer :: [Pair] -> ByteString
okAnswer payload = envelope (["status" .= ("ok" :: Text), "version" .= ("1.16.1" :: Text)] <> payload)

-- | An answer the server refuses the call in, with its own code and reason.
failedAnswerOf :: Int -> Text -> ByteString
failedAnswerOf code reason =
  envelope
    [ "status" .= ("failed" :: Text)
    , "version" .= ("1.16.1" :: Text)
    , "error" .= object ["code" .= code, "message" .= reason]
    ]

envelope :: [Pair] -> ByteString
envelope response =
  LazyByteString.toStrict (Aeson.encode (object ["subsonic-response" .= object response]))
