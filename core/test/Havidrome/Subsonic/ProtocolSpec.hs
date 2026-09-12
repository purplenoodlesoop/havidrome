module Havidrome.Subsonic.ProtocolSpec (spec) where

import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Subsonic.Fixtures
import Havidrome.Subsonic.Protocol
  ( Endpoint (..)
  , audioUrl
  , byAlbumYear
  , byArtistName
  , byTrackOrder
  , decodeAlbums
  , decodeArtists
  , decodePing
  , decodeSongs
  , endpointUrl
  )
import Havidrome.Subsonic.Types
import Test.Hspec

url :: Endpoint -> Text
url = endpointUrl testServer testCredentials testSalt

credentialQuery :: Text
credentialQuery = "u=someone&t=" <> testToken <> "&s=abc123&v=1.16.1&c=havidrome"

spec :: Spec
spec = do
  describe "request URLs" $ do
    it "signs a ping with the salted password hash, never the password" $
      url Ping
        `shouldBe` "https://music.example.org/rest/ping?f=json&" <> credentialQuery

    it "never carries the password itself" $
      testCredentials.password `shouldSatisfy` \password ->
        not (password `Text.isInfixOf` url Ping)

    it "asks for one artist's albums by id" $
      url (GetArtist (ArtistId "a1"))
        `shouldBe` "https://music.example.org/rest/getArtist?id=a1&f=json&" <> credentialQuery

    it "asks for one album's songs by id" $
      url (GetAlbum (AlbumId "b1"))
        `shouldBe` "https://music.example.org/rest/getAlbum?id=b1&f=json&" <> credentialQuery

    it "does not double a slash the server address already ends with" $
      endpointUrl (Server "https://music.example.org/") testCredentials testSalt Ping
        `shouldBe` url Ping

    it "escapes what a query string cannot carry literally" $
      endpointUrl testServer (Credentials "some one&x" "hunter2") testSalt Ping
        `shouldSatisfy` ("u=some%20one%26x&" `Text.isInfixOf`)

  describe "audio requests" $ do
    it "asks for the file the server stores" $
      audioUrl testServer testCredentials testSalt (SongId "s1")
        `shouldBe` "https://music.example.org/rest/stream?id=s1&format=raw&" <> credentialQuery

    it "names no format other than the stored one, and no bit rate" $
      audioUrl testServer testCredentials testSalt (SongId "s1")
        `shouldSatisfy` \request ->
          length (Text.breakOnAll "format=" request) == 1
            && "format=raw" `Text.isInfixOf` request
            && not ("maxBitRate" `Text.isInfixOf` request)

  describe "reading answers" $ do
    it "reads a ping" $
      decodePing pingAnswer `shouldBe` Right ()

    it "reads artists out of the server's index groups" $
      fmap (map (.name)) (decodeArtists artistsAnswer)
        `shouldBe` Right ["zebra", "Aphex Twin", "anohni"]

    it "reads albums, with the year the server gave or none" $
      fmap (map (.year)) (decodeAlbums albumsAnswer)
        `shouldBe` Right [Just 2001, Just 1992, Nothing]

    it "reads songs with their track name and total time" $
      fmap (map (\s -> (s.title, s.duration))) (decodeSongs songsAnswer)
        `shouldBe` Right
          [ ("Pulsewidth", Seconds 228)
          , ("Xtal", Seconds 293)
          , ("Tha", Seconds 543)
          ]

    it "gives a song the server timed at nothing a total time of zero" $
      fmap (map (.duration)) (decodeSongs songWithoutDurationAnswer)
        `shouldBe` Right [Seconds 0]

    it "reads an album the server lists no songs for as empty" $
      decodeSongs emptyAlbumAnswer `shouldBe` Right []

    it "refuses an answer that is not JSON" $
      decodePing "<html>gateway</html>" `shouldSatisfy` isMalformed

    it "refuses an answer missing the payload the call asked for" $
      decodeArtists pingAnswer `shouldSatisfy` isMalformed

  describe "classifying failures" $ do
    it "tells a rejected password from anything else" $
      decodePing wrongPasswordAnswer
        `shouldBe` Left (AuthRejected "Wrong username or password")

    it "keeps a server's other complaints apart from rejected credentials" $
      decodeSongs notFoundAnswer `shouldBe` Left (ServerFailure 70 "Album not found")

    it "survives a failure the server gives no reason for" $
      decodePing unexplainedFailureAnswer `shouldSatisfy` \answer -> case answer of
        Left (ServerFailure _ _) -> True
        _ -> False

  describe "orders" $ do
    it "puts artists in alphabetical order, whatever their capitals" $
      fmap (map (.name) . byArtistName) (decodeArtists artistsAnswer)
        `shouldBe` Right ["anohni", "Aphex Twin", "zebra"]

    it "puts an artist's albums oldest year first" $
      fmap (map (.name) . byAlbumYear) (decodeAlbums albumsAnswer)
        `shouldBe` Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]

    it "puts an album's songs in album order, disc by disc" $
      fmap (map (.title) . byTrackOrder) (decodeSongs songsAnswer)
        `shouldBe` Right ["Xtal", "Tha", "Pulsewidth"]

    it "puts an album the server gave no year for above the oldest, by name" $
      fmap (map (.name) . byAlbumYear) (decodeAlbums albumsWithoutYearAnswer)
        `shouldBe` Right ["Demos", "Tapes", "Live"]

    it "puts a song the server gave no track number for first, by name" $
      fmap (map (.title) . byTrackOrder) (decodeSongs songsWithoutTrackAnswer)
        `shouldBe` Right ["Loose end", "Sketch", "Opener"]

isMalformed :: Either SubsonicError a -> Bool
isMalformed answer = case answer of
  Left (MalformedResponse _) -> True
  _ -> False
