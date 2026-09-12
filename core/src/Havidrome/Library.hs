-- | The library as browsing sees it: the artists to start from, and what lies
-- under an artist and under an album, each fetched only when it is asked for.
--
-- The player's library is a Navidrome server. Anything else that can answer
-- those three questions — a fixed set of lists in a test, say — is as good a
-- library as far as browsing is concerned, which is what the @f@ keeps open.
module Havidrome.Library (Library (..)) where

import Havidrome.Subsonic.Types (Album, AlbumId, Artist, ArtistId, Song)

-- | The three lists browsing walks, in the order it walks them.
data Library f = Library
  { artists :: f [Artist]
  -- ^ Every artist in the library, alphabetically.
  , albums :: ArtistId -> f [Album]
  -- ^ One artist's albums, oldest year first.
  , songs :: AlbumId -> f [Song]
  -- ^ One album's songs, in album order.
  }
