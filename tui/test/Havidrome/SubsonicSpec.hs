-- | The calls the player makes to a Navidrome server, over a transport that
-- answers from a fixture and writes down the address it was asked for, so a
-- spec sees both what came back and what was asked for.
module Havidrome.SubsonicSpec (spec) where

import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Credentials qualified as Credentials
import Havidrome.Library (Library (..))
import Havidrome.Subsonic
import Havidrome.Subsonic.Fixtures
import Test.Hspec

-- | The calls over a transport that answers with the same thing every time,
-- and the address it was last asked for.
stub :: Either SubsonicError ByteString -> IO (Subsonic, IORef Text)
stub answer = do
  asked <- newIORef ""
  let transport = Transport $ \requested -> writeIORef asked requested >> pure answer
  pure (subsonicOver transport testSalt, asked)

answering :: Either SubsonicError ByteString -> (Subsonic -> IO a) -> IO a
answering answer use = stub answer >>= use . fst

-- | The account every call here is made as: the fixtures' server and
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
  (Library (ExceptT SubsonicError IO) -> ExceptT SubsonicError IO a) ->
  Subsonic ->
  IO (Either SubsonicError a)
listing fetch subsonic = runExceptT (fetch (subsonic.browses account))

spec :: Spec
spec = do
  describe "accepting an account" $ do
    it "accepts credentials the server pings back on" $
      answering (Right pingAnswer) (\subsonic -> subsonic.accepts account)
        `shouldReturn` Right ()

    it "reports a rejected password as a refusal" $
      answering (Right wrongPasswordAnswer) (\subsonic -> subsonic.accepts account)
        `shouldReturn` Left (AuthRejected "Wrong username or password")

    it "reports an unreachable host as a network failure, not a refusal" $
      answering
        (Left (NetworkFailure "could not reach the server"))
        (\subsonic -> subsonic.accepts account)
        `shouldReturn` Left (NetworkFailure "could not reach the server")

    it "asks the server to ping" $ do
      (subsonic, asked) <- stub (Right pingAnswer)
      _ <- subsonic.accepts account
      requested <- readIORef asked
      requested `shouldSatisfy` ("/rest/ping?" `Text.isInfixOf`)

  describe "the artists" $ do
    it "hands back every artist, alphabetically" $
      answering (Right artistsAnswer) (fmap (fmap (map (.name))) . listing (.artists))
        `shouldReturn` Right ["anohni", "Aphex Twin", "zebra"]

    it "passes a network failure straight through" $
      answering (Left (NetworkFailure "down")) (listing (.artists))
        `shouldReturn` Left (NetworkFailure "down")

  describe "an artist's albums" $ do
    it "hands back the albums of the artist it asked for, oldest year first" $
      answering
        (Right albumsAnswer)
        (fmap (fmap (map (.name))) . listing (\library -> library.albums (ArtistId "a1")))
        `shouldReturn` Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]

    it "asks for that artist and no other" $ do
      (subsonic, asked) <- stub (Right albumsAnswer)
      _ <- listing (\library -> library.albums (ArtistId "a1")) subsonic
      requested <- readIORef asked
      requested `shouldSatisfy` ("/rest/getArtist?id=a1&" `Text.isInfixOf`)

  describe "an album's songs" $ do
    it "hands back the songs of the album it asked for, in album order" $
      answering
        (Right songsAnswer)
        (fmap (fmap (map (.title))) . listing (\library -> library.songs (AlbumId "b1")))
        `shouldReturn` Right ["Xtal", "Tha", "Pulsewidth"]

    it "asks for that album and no other" $ do
      (subsonic, asked) <- stub (Right songsAnswer)
      _ <- listing (\library -> library.songs (AlbumId "b1")) subsonic
      requested <- readIORef asked
      requested `shouldSatisfy` ("/rest/getAlbum?id=b1&" `Text.isInfixOf`)

  describe "where a song's audio is" $
    it "points at the stored file, signed like every other call" $ do
      (subsonic, _) <- stub (Right pingAnswer)
      subsonic.addresses account (SongId "s1")
        `shouldBe` "https://music.example.org/rest/stream?id=s1&format=raw&u=someone&t="
          <> testToken
          <> "&s=abc123&v=1.16.1&c=havidrome"
