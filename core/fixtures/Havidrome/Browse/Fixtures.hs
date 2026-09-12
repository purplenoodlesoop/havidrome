{- | A library of three artists, held in the specs themselves, so that
browsing can be walked without a server: the lists come back in the order
the client would have put them in, and asking for an artist or an album the
library does not hold yields nothing.
-}
module Havidrome.Browse.Fixtures
  ( library
  , failing
  , artists
  , aphexAlbums
  , drukqsSongs
  , btoumRoumada
  , jynweythek
  , vordhosbn
  , sketchesSongs
  , untitled
  , artist
  , album
  , song
  , unnumbered

    -- * The progress bar
  , bar
  , filledIn
  ) where

import Data.Map.Strict as Map (Map)
import Data.Map.Strict qualified as Map
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Library (Library (Library))
import Havidrome.Library qualified as Library
import Havidrome.Subsonic.Types
  ( Album (..)
  , AlbumId (AlbumId)
  , Artist (..)
  , ArtistId (ArtistId)
  , Seconds (Seconds)
  , Song (..)
  , SongId (SongId)
  , SubsonicError
  )

{- | The whole stand-in library, which never fails. It answers wherever it is
asked: the browsing specs ask it outside 'IO', the screen's specs ask it
alongside a playback session, which is in 'IO'.
-}
library :: (Applicative f) => Library f
library =
  Library
    { Library.artists = pure (Right artists)
    , Library.albums = \wanted -> pure (Right (Map.findWithDefault [] wanted albumsByArtist))
    , Library.songs = \wanted -> pure (Right (Map.findWithDefault [] wanted songsByAlbum))
    }

{- | A library the server never answers for: every one of the three comes back
as the failure asked for here, and never as a list.
-}
failing :: (Applicative f) => SubsonicError -> Library f
failing failure =
  Library
    { Library.artists = pure (Left failure)
    , Library.albums = const (pure (Left failure))
    , Library.songs = const (pure (Left failure))
    }

artists :: [Artist]
artists = [artist "a1" "anohni", artist "a2" "Aphex Twin", artist "a3" "zebra"]

albumsByArtist :: Map ArtistId [Album]
albumsByArtist =
  Map.fromList
    [ (ArtistId "a1", [album "b0" "Hopelessness" (Just 2016), album "b4" "Paradise" (Just 2017)])
    , (ArtistId "a2", aphexAlbums)
    , (ArtistId "a3", [])
    ]

{- | An artist whose albums span the ordering the client settles: the one the
server gives no year for first, then the rest oldest first.
-}
aphexAlbums :: [Album]
aphexAlbums =
  [ album "b1" "Sketches" Nothing
  , album "b2" "Selected Ambient Works 85-92" (Just 1992)
  , album "b3" "Drukqs" (Just 2001)
  ]

songsByAlbum :: Map AlbumId [Song]
songsByAlbum =
  Map.fromList
    [ (AlbumId "b1", sketchesSongs)
    , (AlbumId "b2", [song "s1" "Xtal" 293 (Just 1), song "s2" "Tha" 549 (Just 2), silence])
    , (AlbumId "b3", drukqsSongs)
    , (AlbumId "b4", [song "s7" "Paradise" 222 (Just 1)])
    ]

-- | A song the server gives no length for.
silence :: Song
silence = song "s6" "Silence" 0 (Just 3)

-- | An album of a single song, so that playing it through takes one ending.
sketchesSongs :: [Song]
sketchesSongs = [untitled]

-- | The one song of Sketches.
untitled :: Song
untitled = song "s0" "Untitled" 61 (Just 1)

{- | An album carrying a song the server gives no track number for, which the
client puts first.
-}
drukqsSongs :: [Song]
drukqsSongs = [btoumRoumada, jynweythek, vordhosbn]

{- | The songs of Drukqs by name, in the order the client lists them: the one
with no track number first, then the two the server numbers.
-}
btoumRoumada, jynweythek, vordhosbn :: Song
btoumRoumada = unnumbered "s3" "Btoum Roumada" 96
jynweythek = song "s4" "Jynweythek" 129 (Just 1)
vordhosbn = song "s5" "Vordhosbn" 293 (Just 2)

artist :: Text -> Text -> Artist
artist identifier name = Artist{id = ArtistId identifier, name}

album :: Text -> Text -> Maybe Int -> Album
album identifier name year =
  Album{id = AlbumId identifier, name, year}

song :: Text -> Text -> Int -> Maybe Int -> Song
song identifier name seconds track =
  Song
    { id = SongId identifier
    , title = name
    , duration = Seconds seconds
    , track
    , disc = Nothing
    }

-- | A song the server gives no track number for.
unnumbered :: Text -> Text -> Int -> Song
unnumbered identifier name seconds = song identifier name seconds Nothing

{- | The now-playing overlay's bar with this many of its columns filled, and
this many more empty.
-}
bar :: Int -> Int -> Text
bar filled empty = T.replicate filled "█" <> T.replicate empty "░"

-- | How many columns of the bar on this line are filled.
filledIn :: Text -> Int
filledIn = T.count "█"
