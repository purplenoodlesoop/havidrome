-- | Walking the three levels, over a library held in the specs.
module Havidrome.BrowseSpec (spec) where

import Data.Text (Text)
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
  ( answered
  , aphexAlbums
  , artists
  , drukqsSongs
  , library
  )
import Havidrome.Subsonic.Types (Album (..), Artist (..), Song (..), SubsonicError)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Positive (Positive))

spec :: Spec
spec = do
  describe "atArtists" $ do
    it "opens on the artist list, as the library gave it" $
      items opening `shouldBe` map (.name) artists

    it "starts with the first artist selected" $
      cursor opening `shouldBe` Just 0

  describe "moveUp and moveDown" $ do
    it "move the selection one row at a time" $
      cursor (moveDown (moveDown opening)) `shouldBe` Just 2

    it "leave the list itself alone" $
      items (moveDown opening) `shouldBe` items opening

    prop "keep the selection off the top of the list" $ \(Positive presses) ->
      cursor (times (presses + length artists) moveUp opening) `shouldBe` Just 0

    prop "keep the selection off the bottom of the list" $ \(Positive presses) ->
      cursor (times (presses + length artists) moveDown opening)
        `shouldBe` Just (length artists - 1)

  describe "descend" $ do
    it "shows exactly the selected artist's albums, in the library's order" $
      fmap items (into (moveDown opening)) `shouldBe` Right (map (.name) aphexAlbums)

    it "shows exactly the selected album's songs, in the library's order" $
      fmap items (into (moveDown opening) >>= into . moveDown . moveDown)
        `shouldBe` Right (map (.title) drukqsSongs)

    it "selects the first item of the level it opens" $
      fmap cursor (into (moveDown opening)) `shouldBe` Right (Just 0)

    it "opens an empty level for an artist the library holds no albums for" $
      fmap items (into (moveDown (moveDown opening))) `shouldBe` Right []

    it "stays on a song, which has no level below it" $ do
      let songs = into (moveDown opening) >>= into . moveDown . moveDown
      fmap items (songs >>= into) `shouldBe` fmap items songs

  describe "ascend" $ do
    it "returns from the songs to the albums they belong to" $ do
      let albums = into (moveDown opening)
      fmap (items . ascend) (albums >>= into) `shouldBe` fmap items albums

    it "returns from the albums to the artist list" $
      fmap (items . ascend) (into opening) `shouldBe` Right (items opening)

    it "leaves the level above selecting what was descended into" $
      fmap (cursor . ascend) (into (moveDown opening)) `shouldBe` Right (Just 1)

    it "does nothing on the artist list, which has nothing above it" $
      items (ascend (moveDown opening)) `shouldBe` items opening

    it "keeps the selection a level was left at" $ do
      let albums = fmap moveDown (into (moveDown opening))
      fmap (cursor . ascend) (albums >>= into) `shouldBe` Right (Just 1)

-- | The artist list, as a run opens on it.
opening :: Browse
opening = atArtists artists

-- | One level down, as the stand-in library answers it.
into :: Browse -> Either SubsonicError Browse
into = answered . descend library

-- | How the level on screen reads, item by item.
items :: Browse -> [Text]
items = \case
  AtArtists artists' -> map (.name) artists'.items
  AtAlbums _ albums -> map (.name) albums.items
  AtSongs _ _ songs -> map (.title) songs.items

-- | Which row of the level on screen is selected, and nothing at all when the
-- level is empty and has no row to select.
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
