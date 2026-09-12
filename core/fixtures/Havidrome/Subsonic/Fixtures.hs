{- | Answers copied from the shape Navidrome really sends, and the pieces of a
client the specs point at a stand-in server.
-}
module Havidrome.Subsonic.Fixtures
  ( json
  , testServer
  , testCredentials
  , testSalt
  , testToken
  , artistsAnswer
  , albumsAnswer
  , songsAnswer
  , pingAnswer
  , wrongPasswordAnswer
  , notFoundAnswer
  , unexplainedFailureAnswer
  , albumsWithoutYearAnswer
  , songsWithoutTrackAnswer
  , songWithoutDurationAnswer
  , emptyAlbumAnswer
  ) where

import Data.ByteString (ByteString)
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Havidrome.Subsonic.Protocol (Salt, mkSalt)
import Havidrome.Subsonic.Types (Credentials (..), Server (..))

{- | Reads a fixture written with @'@ where JSON wants @"@, so the fixtures
stay legible as Haskell string literals.
-}
json :: Text -> ByteString
json = T.encodeUtf8 . T.map (\c -> if c == '\'' then '"' else c)

testServer :: Server
testServer = Server "https://music.example.org"

testCredentials :: Credentials
testCredentials = Credentials{user = "someone", password = "hunter2"}

testSalt :: Salt
testSalt = mkSalt "abc123"

{- | @md5("hunter2" <> "abc123")@, hex-encoded — what the server compares
against, pinned here so the auth scheme cannot drift unnoticed.
-}
testToken :: Text
testToken = "c402b3eac5900b52527b1f83f2fc94b3"

-- | Artists arrive grouped under index letters, and in no useful order.
artistsAnswer :: ByteString
artistsAnswer =
  json
    "{'subsonic-response':{'status':'ok','version':'1.16.1','artists':{\
    \'ignoredArticles':'The El La','index':[\
    \{'name':'Z','artist':[{'id':'a3','name':'zebra','albumCount':1}]},\
    \{'name':'A','artist':[\
    \{'id':'a1','name':'Aphex Twin','albumCount':3},\
    \{'id':'a2','name':'anohni','albumCount':1}]}]}}}"

-- | One artist's albums, out of order, one of them with no year recorded.
albumsAnswer :: ByteString
albumsAnswer =
  json
    "{'subsonic-response':{'status':'ok','version':'1.16.1','artist':{\
    \'id':'a1','name':'Aphex Twin','albumCount':3,'album':[\
    \{'id':'b2','name':'Drukqs','artistId':'a1','year':2001,'songCount':30},\
    \{'id':'b1','name':'Selected Ambient Works 85-92','artistId':'a1','year':1992,'songCount':13},\
    \{'id':'b3','name':'Sketches','artistId':'a1','songCount':4}]}}}"

-- | One album's songs, out of order, across two discs.
songsAnswer :: ByteString
songsAnswer =
  json
    "{'subsonic-response':{'status':'ok','version':'1.16.1','album':{\
    \'id':'b1','name':'Selected Ambient Works 85-92','song':[\
    \{'id':'s3','title':'Pulsewidth','duration':228,'track':1,'discNumber':2,'suffix':'flac'},\
    \{'id':'s1','title':'Xtal','duration':293,'track':1,'discNumber':1,'suffix':'flac'},\
    \{'id':'s2','title':'Tha','duration':543,'track':2,'discNumber':1,'suffix':'flac'}]}}}"

pingAnswer :: ByteString
pingAnswer =
  json
    "{'subsonic-response':{'status':'ok','version':'1.16.1','type':'navidrome',\
    \'serverVersion':'0.53.3','openSubsonic':true}}"

wrongPasswordAnswer :: ByteString
wrongPasswordAnswer =
  json
    "{'subsonic-response':{'status':'failed','version':'1.16.1',\
    \'error':{'code':40,'message':'Wrong username or password'}}}"

notFoundAnswer :: ByteString
notFoundAnswer =
  json
    "{'subsonic-response':{'status':'failed','version':'1.16.1',\
    \'error':{'code':70,'message':'Album not found'}}}"

unexplainedFailureAnswer :: ByteString
unexplainedFailureAnswer =
  json "{'subsonic-response':{'status':'failed','version':'1.16.1'}}"

-- | One artist's albums, two of which the server gives no year for.
albumsWithoutYearAnswer :: ByteString
albumsWithoutYearAnswer =
  json
    "{'subsonic-response':{'status':'ok','artist':{'id':'a4','name':'Nadia','album':[\
    \{'id':'b5','name':'Live','artistId':'a4','year':1998},\
    \{'id':'b7','name':'Tapes','artistId':'a4'},\
    \{'id':'b6','name':'Demos','artistId':'a4'}]}}}"

-- | Songs of one album, two of which the server gives no track number for.
songsWithoutTrackAnswer :: ByteString
songsWithoutTrackAnswer =
  json
    "{'subsonic-response':{'status':'ok','album':{'id':'b4','name':'Tape','song':[\
    \{'id':'s1','title':'Opener','duration':120,'track':1},\
    \{'id':'s3','title':'Sketch','duration':60},\
    \{'id':'s2','title':'Loose end','duration':90}]}}}"

-- | A song the server gives no duration for.
songWithoutDurationAnswer :: ByteString
songWithoutDurationAnswer =
  json
    "{'subsonic-response':{'status':'ok','album':{'id':'b9','name':'Odds',\
    \'song':[{'id':'s9','title':'Untitled','track':1}]}}}"

-- | An album the server reports no songs for at all.
emptyAlbumAnswer :: ByteString
emptyAlbumAnswer =
  json "{'subsonic-response':{'status':'ok','album':{'id':'b9','name':'Odds'}}}"
