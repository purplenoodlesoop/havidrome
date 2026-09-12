-- | Walking the three levels, over a library held in the tests.
module Havidrome.BrowseTest (tests) where

import Data.Functor.Identity (runIdentity)
import Data.Text as T (Text)
import Havidrome.Browse
  ( Browse (AtAlbums, AtArtists, AtSongs)
  , Rows (..)
  , ascend
  , atArtists
  , descend
  , moveDown
  , moveUp
  )
import Havidrome.Browse.Fixtures
  ( aphexAlbums
  , artists
  , drukqsSongs
  , library
  )
import Havidrome.Check (Checks, example)
import Havidrome.Subsonic.Types (Album (..), Artist (..), Song (..), SubsonicError)
import Hedgehog (Group (Group), forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests = Group "Havidrome.Browse" (artistList <> moving <> descending <> ascending <> ends)

-- | The artist list browsing opens on.
artistList :: Checks
artistList =
  [
    ( "atArtists opens on the artist list, as the library gave it"
    , example (items opening === fmap (.name) artists)
    )
  ,
    ( "atArtists starts with the first artist selected"
    , example (cursor opening === Just 0)
    )
  ]

-- | Moving the selection up and down a list.
moving :: Checks
moving =
  [
    ( "moveUp and moveDown move the selection one row at a time"
    , example (cursor (moveDown (moveDown opening)) === Just 2)
    )
  ,
    ( "moveUp and moveDown leave the list itself alone"
    , example (items (moveDown opening) === items opening)
    )
  ]

-- | Going down into what the selected row holds.
descending :: Checks
descending =
  [
    ( "descend shows exactly the selected artist's albums, in the library's order"
    , example (fmap items (into (moveDown opening)) === Right (fmap (.name) aphexAlbums))
    )
  ,
    ( "descend shows exactly the selected album's songs, in the library's order"
    , example
        ( fmap items (into (moveDown opening) >>= into . moveDown . moveDown)
            === Right (fmap (.title) drukqsSongs)
        )
    )
  ,
    ( "descend selects the first item of the level it opens"
    , example (fmap cursor (into (moveDown opening)) === Right (Just 0))
    )
  ,
    ( "descend opens an empty level for an artist the library holds no albums for"
    , example (fmap items (into (moveDown (moveDown opening))) === Right [])
    )
  ,
    ( "descend stays on a song, which has no level below it"
    , example do
        let songs = into (moveDown opening) >>= into . moveDown . moveDown
        fmap items (songs >>= into) === fmap items songs
    )
  ]

-- | Coming back up to the list that was descended from.
ascending :: Checks
ascending =
  [
    ( "ascend returns from the songs to the albums they belong to"
    , example do
        let albums = into (moveDown opening)
        fmap (items . ascend) (albums >>= into) === fmap items albums
    )
  ,
    ( "ascend returns from the albums to the artist list"
    , example (fmap (items . ascend) (into opening) === Right (items opening))
    )
  ,
    ( "ascend leaves the level above selecting what was descended into"
    , example (fmap (cursor . ascend) (into (moveDown opening)) === Right (Just 1))
    )
  ,
    ( "ascend does nothing on the artist list, which has nothing above it"
    , example (items (ascend (moveDown opening)) === items opening)
    )
  ,
    ( "ascend keeps the selection a level was left at"
    , example do
        let albums = fmap moveDown (into (moveDown opening))
        fmap (cursor . ascend) (albums >>= into) === Right (Just 1)
    )
  ]

-- | Where the selection stops, at either end of a list.
ends :: Checks
ends =
  [
    ( "moveUp keeps the selection off the top of the list, however long it is held"
    , property do
        presses <- forAll pressing
        cursor (times (presses + length artists) moveUp opening) === Just 0
    )
  ,
    ( "moveDown keeps the selection off the bottom of the list, however long it is held"
    , property do
        presses <- forAll pressing
        cursor (times (presses + length artists) moveDown opening)
          === Just (length artists - 1)
    )
  ]
 where
  pressing = Gen.int (Range.linear 1 20)

-- | The artist list, as a run opens on it.
opening :: Browse
opening = atArtists artists

-- | One level down, as the stand-in library answers it.
into :: Browse -> Either SubsonicError Browse
into = runIdentity . descend library

-- | How the level on screen reads, item by item.
items :: Browse -> [Text]
items = \case
  AtArtists artists' -> fmap (.name) artists'.items
  AtAlbums _ albums -> fmap (.name) albums.items
  AtSongs _ _ songs -> fmap (.title) songs.items

{- | Which row of the level on screen is selected, and nothing at all when the
level is empty and has no row to select.
-}
cursor :: Browse -> Maybe Int
cursor = \case
  AtArtists artists' -> at artists'
  AtAlbums _ albums -> at albums
  AtSongs _ _ songs -> at songs
 where
  at :: Rows a -> Maybe Int
  at level = if null level.items then Nothing else Just level.place

times :: Int -> (a -> a) -> a -> a
times count move = foldr (.) id (replicate count move)
