-- | The library as browsing sees it: the artists to start from, and what lies
-- under an artist and under an album, each fetched only when it is asked for.
--
-- Every one of the three can fail, and says so in its own answer: a list, or
-- the 'SubsonicError' that stopped it. A failure is never half a list, and it
-- travels as data rather than in a transformer, so browsing handles it where
-- it asked.
--
-- The player's library is a Navidrome server. Anything else that can answer
-- those three questions — a fixed set of lists in a test, say — is as good a
-- library as far as browsing is concerned, which is what the @f@ keeps open.
module Havidrome.Library (Library (..)) where

import Havidrome.Subsonic.Types
  ( Album
  , AlbumId
  , Artist
  , ArtistId
  , Song
  , SubsonicError
  )

-- | The three lists browsing walks, in the order it walks them.
data Library f = Library
  { artists :: f (Either SubsonicError [Artist])
  -- ^ Every artist in the library, alphabetically.
  , albums :: ArtistId -> f (Either SubsonicError [Album])
  -- ^ One artist's albums, oldest year first.
  , songs :: AlbumId -> f (Either SubsonicError [Song])
  -- ^ One album's songs, in album order.
  }
