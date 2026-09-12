-- | The calls the player makes to a Navidrome server, over a transport that
-- answers from a fixture and writes down the address it was asked for, so a
-- test sees both what came back and what was asked for.
module Havidrome.SubsonicTest (tests) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
import Havidrome.Credentials qualified as Credentials
import Havidrome.Library (Library (..))
import Havidrome.Subsonic
import Havidrome.Subsonic.Fixtures
import Hedgehog (Gen, Group (Group), assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Subsonic"
    ( accepting
        <> listings
        <> audio
        <> anyCall
    )

-- | The credential check, and what it makes of a server that refuses.
accepting :: Checks
accepting =
  [
    ( "accepts credentials the server pings back on"
    , example do
        answer <- evalIO (answering (Right pingAnswer) (\subsonic -> subsonic.accepts account))
        answer === Right ()
    )
  ,
    ( "reports a rejected password as a refusal"
    , example do
        answer <-
          evalIO (answering (Right wrongPasswordAnswer) (\subsonic -> subsonic.accepts account))
        answer === Left (AuthRejected "Wrong username or password")
    )
  ,
    ( "reports an unreachable host as a network failure, not a refusal"
    , example do
        let down = NetworkFailure "could not reach the server"
        answer <- evalIO (answering (Left down) (\subsonic -> subsonic.accepts account))
        answer === Left down
    )
  ]

-- | The three listings, and what each asks the server for.
listings :: Checks
listings =
  [
    ( "asks the server to ping"
    , example do
        requested <- evalIO do
          (subsonic, asked) <- stub (Right pingAnswer)
          _ <- subsonic.accepts account
          readIORef asked
        assert ("/rest/ping?" `T.isInfixOf` requested)
    )
  ,
    ( "hands back every artist, alphabetically"
    , example do
        named <-
          evalIO (answering (Right artistsAnswer) (fmap (fmap (fmap (.name))) . listing (.artists)))
        named === Right ["anohni", "Aphex Twin", "zebra"]
    )
  ,
    ( "passes a network failure straight through"
    , example do
        answer <- evalIO (answering (Left (NetworkFailure "down")) (listing (.artists)))
        fmap (fmap (.name)) answer === Left (NetworkFailure "down")
    )
  ,
    ( "hands back the albums of the artist it asked for, oldest year first"
    , example do
        named <-
          evalIO . answering (Right albumsAnswer) $
            fmap (fmap (fmap (.name))) . listing (\library -> library.albums (ArtistId "a1"))
        named === Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]
    )
  ,
    ( "asks for that artist and no other"
    , example do
        requested <- evalIO do
          (subsonic, asked) <- stub (Right albumsAnswer)
          _ <- listing (\library -> library.albums (ArtistId "a1")) subsonic
          readIORef asked
        assert ("/rest/getArtist?id=a1&" `T.isInfixOf` requested)
    )
  ,
    ( "hands back the songs of the album it asked for, in album order"
    , example do
        titled <-
          evalIO . answering (Right songsAnswer) $
            fmap (fmap (fmap (.title))) . listing (\library -> library.songs (AlbumId "b1"))
        titled === Right ["Xtal", "Tha", "Pulsewidth"]
    )
  ,
    ( "asks for that album and no other"
    , example do
        requested <- evalIO do
          (subsonic, asked) <- stub (Right songsAnswer)
          _ <- listing (\library -> library.songs (AlbumId "b1")) subsonic
          readIORef asked
        assert ("/rest/getAlbum?id=b1&" `T.isInfixOf` requested)
    )
  ]

-- | The file a song is played from.
audio :: Checks
audio =
  [
    ( "points at the stored file, signed like every other call"
    , example do
        (subsonic, _) <- evalIO (stub (Right pingAnswer))
        subsonic.addresses account (SongId "s1")
          === "https://music.example.org/rest/stream?id=s1&format=raw&u=someone&t="
            <> testToken
            <> "&s=abc123&v=1.16.1&c=havidrome"
    )
  ]

-- | What holds of every call the player makes, whatever it is.
anyCall :: Checks
anyCall =
  [
    ( "passes whatever failure the transport reports straight through, on every call"
    , property do
        failure <- forAll trouble
        asking <- forAll call
        answer <- evalIO (answering (Left failure) (asking.make account))
        answer === Left failure
    )
  ,
    ( "addresses every call to the server the account names, and no other"
    , property do
        (host, them) <- forAll anAccount
        asking <- forAll call
        requested <- evalIO do
          (subsonic, asked) <- stub (Right pingAnswer)
          _ <- asking.make them subsonic
          readIORef asked
        assert ((host <> "/rest/") `T.isPrefixOf` requested)
    )
  ,
    ( "asks for a song's audio under that server too, naming the song and the stored file"
    , property do
        (host, them) <- forAll anAccount
        identifier <- forAll songId
        (subsonic, _) <- evalIO (stub (Right pingAnswer))
        let wanted = host <> "/rest/stream?id=" <> identifier <> "&format=raw&"
        assert (wanted `T.isPrefixOf` subsonic.addresses them (SongId identifier))
    )
  ]

-- | The calls over a transport that answers with the same thing every time,
-- and the address it was last asked for.
stub :: Either SubsonicError ByteString -> IO (Subsonic, IORef Text)
stub answer = do
  asked <- newIORef ""
  let transport = Transport $ \requested -> writeIORef asked requested >> pure answer
  pure (subsonicOver transport testSalt, asked)

answering :: Either SubsonicError ByteString -> (Subsonic -> IO a) -> IO a
answering answer use = stub answer >>= use . fst

-- | The account every example here is made as: the fixtures' server and
-- credentials, in the shape the config file holds them.
account :: Credentials.Credentials
account =
  Credentials.Credentials
    { server = testServer.url
    , username = testCredentials.user
    , password = testCredentials.password
    }

-- | One of the three lists of that account's library, fetched.
listing ::
  (Library IO -> IO (Either SubsonicError a)) ->
  Subsonic ->
  IO (Either SubsonicError a)
listing fetch subsonic = fetch (subsonic.browses account)

-- | One of the calls the player makes, made as whatever account it is handed
-- and reduced to whether it worked, so that any of the four stands where any
-- other does.
data Call = Call
  { named :: Text
  , make :: Credentials.Credentials -> Subsonic -> IO (Either SubsonicError ())
  }

instance Show Call where
  show asking = T.unpack asking.named

-- | Every failure a transport can report, whatever it says about itself.
trouble :: Gen SubsonicError
trouble =
  Gen.choice
    [ NetworkFailure <$> saying
    , AuthRejected <$> saying
    , ServerFailure <$> Gen.int (Range.linear 100 599) <*> saying
    ]
 where
  saying = Gen.text (Range.linear 0 24) Gen.unicode

-- | Any of the four calls: the ping and each of the three listings.
call :: Gen Call
call =
  Gen.element
    [ Call "the ping" (\them subsonic -> subsonic.accepts them)
    , Call "the artists" (\them subsonic -> nothingBack (subsonic.browses them).artists)
    , Call "an artist's albums" (\them subsonic -> nothingBack ((subsonic.browses them).albums (ArtistId "a1")))
    , Call "an album's songs" (\them subsonic -> nothingBack ((subsonic.browses them).songs (AlbumId "b1")))
    ]
 where
  nothingBack = fmap void

-- | An account on some server, named and signed in however: the server's own
-- address beside it, because that is what a call must be addressed under.
anAccount :: Gen (Text, Credentials.Credentials)
anAccount = do
  host <-
    Gen.element
      ["https://music.example.org", "http://box.example:4533", "https://a.example/sub"]
  user <- Gen.text (Range.linear 1 12) Gen.alphaNum
  secret <- Gen.text (Range.linear 1 16) Gen.unicode
  pure (host, Credentials.Credentials {server = host, username = user, password = secret})

-- | The id a song is known by on a server.
songId :: Gen Text
songId = Gen.text (Range.linear 1 8) Gen.alphaNum
