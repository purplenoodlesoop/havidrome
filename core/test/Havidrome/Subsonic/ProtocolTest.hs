-- | The calls the client makes and the answers it reads back: the URL each
-- one is asked at, what the payload becomes, and the orders the lists are put
-- into.
module Havidrome.Subsonic.ProtocolTest (tests) where

import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Check (example)
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
  ( Endpoint (GetAlbum, GetArtist, Ping)
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
import Hedgehog (Group (Group), assert, (===))

tests :: Group
tests =
  Group
    "Havidrome.Subsonic.Protocol"
    [
      ( "a ping is signed with the salted password hash, never the password"
      , example (url Ping === "https://music.example.org/rest/ping?f=json&" <> credentialQuery)
      )
    ,
      ( "a request never carries the password itself"
      , example (assert (not (testCredentials.password `Text.isInfixOf` url Ping)))
      )
    ,
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
      ( "a slash the server address already ends with is not doubled"
      , example
          ( endpointUrl (Server "https://music.example.org/") testCredentials testSalt Ping
              === url Ping
          )
      )
    ,
      ( "what a query string cannot carry literally is escaped"
      , example
          ( assert
              ( "u=some%20one%26x&"
                  `Text.isInfixOf` endpointUrl testServer (Credentials "some one&x" "hunter2") testSalt Ping
              )
          )
      )
    ,
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
          length (Text.breakOnAll "format=" request) === 1
          assert ("format=raw" `Text.isInfixOf` request)
          assert (not ("maxBitRate" `Text.isInfixOf` request))
      )
    ,
      ( "a ping is read"
      , example (decodePing pingAnswer === Right ())
      )
    ,
      ( "artists are read out of the server's index groups"
      , example
          ( fmap (map (.name)) (decodeArtists artistsAnswer)
              === Right ["zebra", "Aphex Twin", "anohni"]
          )
      )
    ,
      ( "albums are read, with the year the server gave or none"
      , example
          ( fmap (map (.year)) (decodeAlbums albumsAnswer)
              === Right [Just 2001, Just 1992, Nothing]
          )
      )
    ,
      ( "songs are read with their track name and total time"
      , example
          ( fmap (map (\s -> (s.title, s.duration))) (decodeSongs songsAnswer)
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
          ( fmap (map (.duration)) (decodeSongs songWithoutDurationAnswer)
              === Right [Seconds 0]
          )
      )
    ,
      ( "an album the server lists no songs for is read as empty"
      , example (decodeSongs emptyAlbumAnswer === Right [])
      )
    ,
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
      ( "a failure the server gives no reason for is survived"
      , example
          ( assert case decodePing unexplainedFailureAnswer of
              Left (ServerFailure _ _) -> True
              _ -> False
          )
      )
    ,
      ( "artists are put in alphabetical order, whatever their capitals"
      , example
          ( fmap (map (.name) . byArtistName) (decodeArtists artistsAnswer)
              === Right ["anohni", "Aphex Twin", "zebra"]
          )
      )
    ,
      ( "an artist's albums are put oldest year first"
      , example
          ( fmap (map (.name) . byAlbumYear) (decodeAlbums albumsAnswer)
              === Right ["Sketches", "Selected Ambient Works 85-92", "Drukqs"]
          )
      )
    ,
      ( "an album's songs are put in album order, disc by disc"
      , example
          ( fmap (map (.title) . byTrackOrder) (decodeSongs songsAnswer)
              === Right ["Xtal", "Tha", "Pulsewidth"]
          )
      )
    ,
      ( "an album the server gave no year for is put above the oldest, by name"
      , example
          ( fmap (map (.name) . byAlbumYear) (decodeAlbums albumsWithoutYearAnswer)
              === Right ["Demos", "Tapes", "Live"]
          )
      )
    ,
      ( "a song the server gave no track number for is put first, by name"
      , example
          ( fmap (map (.title) . byTrackOrder) (decodeSongs songsWithoutTrackAnswer)
              === Right ["Loose end", "Sketch", "Opener"]
          )
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
