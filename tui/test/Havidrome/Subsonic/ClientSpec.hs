module Havidrome.Subsonic.ClientSpec (spec) where

import Data.ByteString (ByteString)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Subsonic
import Havidrome.Subsonic.Fixtures
import Test.Hspec

-- | A client whose transport answers with the same thing every time, and
-- remembers the last address it was asked for.
stubClient :: Either SubsonicError ByteString -> IO (Client, IORef Text)
stubClient answer = do
  asked <- newIORef ""
  let transport = Transport $ \requested -> writeIORef asked requested >> pure answer
  pure (clientOver transport testSalt testServer testCredentials, asked)

answering :: Either SubsonicError ByteString -> (Client -> IO a) -> IO a
answering answer use = stubClient answer >>= use . fst

spec :: Spec
spec = do
  checking
  artistList
  albumList
  songList
  audioUrls

checking :: Spec
checking = describe "checkCredentials" $ do
  it "accepts credentials the server pings back on" $
    answering (Right pingAnswer) checkCredentials `shouldReturn` Right ()

  it "reports a rejected password as a refusal" $
    answering (Right wrongPasswordAnswer) checkCredentials
      `shouldReturn` Left (AuthRejected "Wrong username or password")

  it "reports an unreachable host as a network failure, not a refusal" $
    answering (Left (NetworkFailure "could not reach the server")) checkCredentials
      `shouldReturn` Left (NetworkFailure "could not reach the server")

  it "asks the server to ping" $ do
    (client, asked) <- stubClient (Right pingAnswer)
    _ <- checkCredentials client
    requested <- readIORef asked
    requested `shouldSatisfy` ("/rest/ping?" `T.isInfixOf`)

artistList :: Spec
artistList = describe "listArtists" $ do
  it "hands back every artist, alphabetically" $
    answering (Right artistsAnswer) (fmap (fmap (fmap (.name))) . listArtists)
      `shouldReturn` Right ["anohni", "Aphex Twin", "zebra"]

  it "passes a network failure straight through" $
    answering (Left (NetworkFailure "down")) listArtists
      `shouldReturn` Left (NetworkFailure "down")

albumList :: Spec
albumList = describe "listAlbums" $ do
  it "hands back the albums of the artist it asked for, oldest year first" $
    answering
      (Right albumsAnswer)
      (\client -> fmap (fmap (fmap (.name))) (listAlbums client (ArtistId "a1")))
      `shouldReturn` Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]

  it "asks for that artist and no other" $ do
    (client, asked) <- stubClient (Right albumsAnswer)
    _ <- listAlbums client (ArtistId "a1")
    requested <- readIORef asked
    requested `shouldSatisfy` ("/rest/getArtist?id=a1&" `T.isInfixOf`)

songList :: Spec
songList = describe "listSongs" $ do
  it "hands back the songs of the album it asked for, in album order" $
    answering
      (Right songsAnswer)
      (\client -> fmap (fmap (fmap (.title))) (listSongs client (AlbumId "b1")))
      `shouldReturn` Right ["Xtal", "Tha", "Pulsewidth"]

  it "asks for that album and no other" $ do
    (client, asked) <- stubClient (Right songsAnswer)
    _ <- listSongs client (AlbumId "b1")
    requested <- readIORef asked
    requested `shouldSatisfy` ("/rest/getAlbum?id=b1&" `T.isInfixOf`)

audioUrls :: Spec
audioUrls = describe "songAudioUrl" $
  it "points at the stored file, signed like every other call" $ do
    (client, _) <- stubClient (Right pingAnswer)
    songAudioUrl client (SongId "s1")
      `shouldBe` "https://music.example.org/rest/stream?id=s1&format=raw&u=someone&t="
        <> testToken
        <> "&s=abc123&v=1.16.1&c=havidrome"

