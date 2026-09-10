-- | The library as browsing sees it: the artists to start from, and what lies
-- under an artist and under an album, each fetched only when it is asked for.
--
-- The player's library is a Navidrome server ('subsonic'). Anything else that
-- can answer those three questions — a fixed set of lists in a test, say — is
-- as good a library as far as browsing is concerned, which is what the @f@
-- keeps open.
module Havidrome.Library
  ( Library (..)
  , subsonic
  ) where

import Control.Monad.Trans.Except (ExceptT (ExceptT))
import Havidrome.Subsonic
  ( Album
  , AlbumId
  , Artist
  , ArtistId
  , Client
  , Song
  , SubsonicError
  , listAlbums
  , listArtists
  , listSongs
  )

-- | The three lists browsing walks, in the order it walks them.
data Library f = Library
  { artists :: f [Artist]
  -- ^ Every artist in the library, alphabetically.
  , albums :: ArtistId -> f [Album]
  -- ^ One artist's albums, oldest year first.
  , songs :: AlbumId -> f [Song]
  -- ^ One album's songs, in album order.
  }

-- | The library a Navidrome server holds. Every call can fail, and a failure
-- stops the fetch it was part of rather than yielding a half-list, which is
-- what the 'ExceptT' says.
subsonic :: Client -> Library (ExceptT SubsonicError IO)
subsonic client =
  Library
    { artists = ExceptT (listArtists client)
    , albums = ExceptT . listAlbums client
    , songs = ExceptT . listSongs client
    }
