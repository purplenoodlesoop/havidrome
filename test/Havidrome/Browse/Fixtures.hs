{-# LANGUAGE OverloadedStrings #-}

-- | A library of three artists, held in the specs themselves, so that
-- browsing can be walked without a server: the lists come back in the order
-- the client would have put them in, and asking for an artist or an album the
-- library does not hold yields nothing.
module Havidrome.Browse.Fixtures
  ( Answer
  , answered
  , library
  , failing
  , artists
  , aphexAlbums
  , drukqsSongs
  , artist
  , album
  , song
  , unnumbered
  ) where

import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Functor.Identity (Identity, runIdentity)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Havidrome.Library (Library (Library))
import Havidrome.Library qualified as Library
import Havidrome.Subsonic
  ( Album (..)
  , AlbumId (AlbumId)
  , Artist (..)
  , ArtistId (ArtistId)
  , Seconds (Seconds)
  , Song (..)
  , SongId (SongId)
  , SubsonicError
  )

-- | What the stand-in library says: at once, and either an answer or the
-- failure the specs asked it for.
type Answer = ExceptT SubsonicError Identity

-- | What it said.
answered :: Answer a -> Either SubsonicError a
answered = runIdentity . runExceptT

-- | The whole stand-in library, which never fails.
library :: Library Answer
library =
  Library
    { Library.artists = pure artists
    , Library.albums = \wanted -> pure (Map.findWithDefault [] wanted albumsByArtist)
    , Library.songs = \wanted -> pure (Map.findWithDefault [] wanted songsByAlbum)
    }

-- | A library the server never answers for.
failing :: SubsonicError -> Library Answer
failing failure =
  Library
    { Library.artists = throwE failure
    , Library.albums = const (throwE failure)
    , Library.songs = const (throwE failure)
    }

artists :: [Artist]
artists = [artist "a1" "anohni", artist "a2" "Aphex Twin", artist "a3" "zebra"]

albumsByArtist :: Map ArtistId [Album]
albumsByArtist =
  Map.fromList
    [ (ArtistId "a1", [album "b0" "Hopelessness" (Just 2016)])
    , (ArtistId "a2", aphexAlbums)
    , (ArtistId "a3", [])
    ]

-- | An artist whose albums span the ordering the client settles: the one the
-- server gives no year for first, then the rest oldest first.
aphexAlbums :: [Album]
aphexAlbums =
  [ album "b1" "Sketches" Nothing
  , album "b2" "Selected Ambient Works 85-92" (Just 1992)
  , album "b3" "Drukqs" (Just 2001)
  ]

songsByAlbum :: Map AlbumId [Song]
songsByAlbum =
  Map.fromList
    [ (AlbumId "b1", [song "s0" "Untitled" 61 (Just 1)])
    , (AlbumId "b2", [song "s1" "Xtal" 293 (Just 1), song "s2" "Tha" 549 (Just 2)])
    , (AlbumId "b3", drukqsSongs)
    ]

-- | An album carrying a song the server gives no track number for, which the
-- client puts first.
drukqsSongs :: [Song]
drukqsSongs =
  [ unnumbered "s3" "Btoum Roumada" 96
  , song "s4" "Jynweythek" 129 (Just 1)
  , song "s5" "Vordhosbn" 293 (Just 2)
  ]

artist :: Text -> Text -> Artist
artist identifier name = Artist {artistId = ArtistId identifier, artistName = name}

album :: Text -> Text -> Maybe Int -> Album
album identifier name year =
  Album {albumId = AlbumId identifier, albumName = name, albumYear = year}

song :: Text -> Text -> Int -> Maybe Int -> Song
song identifier name seconds track =
  Song
    { songId = SongId identifier
    , songTitle = name
    , songDuration = Seconds seconds
    , songTrack = track
    , songDisc = Nothing
    }

-- | A song the server gives no track number for.
unnumbered :: Text -> Text -> Int -> Song
unnumbered identifier name seconds = song identifier name seconds Nothing
