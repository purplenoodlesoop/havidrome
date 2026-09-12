-- | The whole of a playback session, driven against a stand-in backend: no
-- audio device, no server, and nothing of the terminal.
module Havidrome.PlaybackSpec (spec) where

import Control.Monad (replicateM)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Audio.State (Failure (..), Motion (..))
import Havidrome.Playback
import Havidrome.Playback.Standin
import Havidrome.Subsonic.Types (Seconds (..), Song (..), SongId (..))
import Test.Hspec

-- | An album of so many songs, in album order, told apart from another
-- album's by the name its songs are lettered with.
album :: Text -> Int -> [Song]
album name count = fmap (song name) [1 .. count]

-- | One song of such an album, by its track number, counting from one.
song :: Text -> Int -> Song
song name number =
  Song
    { id = SongId (name <> T.pack (show number))
    , title = name <> T.pack (" track " <> show number)
    , duration = Seconds 180
    , track = Just number
    , disc = Nothing
    }

-- | Plays the album from that song of it on, which is what picking the song
-- with Enter does. A song the album does not hold is not one a spec picks,
-- and asking for one fails the spec there.
picking :: Session -> [Song] -> Song -> IO ()
picking session whole wanted =
  maybe
    (expectationFailure "the album has no such song")
    (start session)
    (startingAt whole wanted.id)

-- | What the backend is told when that song is played from its beginning.
from :: Song -> (Text, Seconds)
from wanted = (address wanted.id, Seconds 0)

-- | Nothing is asked of the session: the audio simply runs out, so many
-- times, and the session is handed what the backend says about it. This is
-- the whole of \"without further input\".
runOut :: Standin -> Session -> Int -> IO [Failure]
runOut standin session times =
  fmap concat . replicateM times $ do
    finish standin
    attend session

-- | The song a session is playing, if it is playing one.
current :: Session -> IO (Maybe Song)
current = fmap (fmap (.song)) . nowPlaying

-- | The album every spec here plays, and its four songs by name.
tracks :: [Song]
tracks = album "a" 4

first, second, third, fourth :: Song
first = song "a" 1
second = song "a" 2
third = song "a" 3
fourth = song "a" 4

spec :: Spec
spec = do
  picked
  arrivals
  anotherAlbum
  movingOn
  movingBack
  unplayable
  unreachable
  held
  stopped

picked :: Spec
picked = describe "picking a song" $ do
  it "plays that song, from its beginning" $ withStandin $ \standin session -> do
    picking session tracks second
    loaded standin `shouldReturn` [from second]

  it "then plays the rest of its album, in album order, with nothing more asked" $
    withStandin $ \standin session -> do
      picking session tracks second
      shown <- runOut standin session 3
      shown `shouldBe` []
      loaded standin `shouldReturn` fmap from [second, third, fourth]

  it "plays nothing outside that album, however long it is left alone" $
    withStandin $ \standin session -> do
      picking session tracks first
      _ <- runOut standin session 8
      loaded standin `shouldReturn` fmap from tracks

  it "leaves nothing playing once the album has run out" $
    withStandin $ \standin session -> do
      picking session tracks third
      _ <- runOut standin session 2
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing

arrivals :: Spec
arrivals = describe "how the song playing came to be playing" $ do
  it "is picked for a song the album was started from, loading until its audio starts" $
    withStandin $ \standin session -> do
      picking session tracks second
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 0) Picked Loading)
      begin standin
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 0) Picked (Sounding Running))

  it "is picked, and loading, for a song picked while another still loads" $
    withStandin $ \_ session -> do
      picking session tracks second
      picking session tracks third
      nowPlaying session `shouldReturn` Just (Playing third (Seconds 0) Picked Loading)

  it "is followed for the song the album runs on to" $ withStandin $ \standin session -> do
    picking session tracks second
    begin standin
    _ <- runOut standin session 1
    nowPlaying session `shouldReturn` Just (Playing third (Seconds 0) Followed Loading)

  it "is followed for the songs next and previous move to" $
    withStandin $ \_ session -> do
      picking session tracks second
      next session
      nowPlaying session `shouldReturn` Just (Playing third (Seconds 0) Followed Loading)
      previous session
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 0) Followed Loading)

  it "is followed for the song a skipped one gives way to" $
    withStandin $ \standin session -> do
      picking session tracks second
      breakWith standin (Unplayable "the file will not play: unrecognized file format")
      _ <- attend session
      fmap (.arrival) <$> nowPlaying session `shouldReturn` Just Followed

  it "leaves a picked song held while it loads held at its start once loaded" $
    withStandin $ \standin session -> do
      picking session tracks second
      togglePause session
      begin standin
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 0) Picked (Sounding Paused))
      motionOf standin `shouldReturn` Just Paused

anotherAlbum :: Spec
anotherAlbum = describe "picking a song of another album" $
  it "replaces what is playing, and then goes on through that album" $
    withStandin $ \standin session -> do
      let other = album "b" 3
      picking session tracks first
      picking session other (song "b" 2)
      _ <- runOut standin session 2
      current session `shouldReturn` Nothing
      loaded standin `shouldReturn` (from first : fmap from [song "b" 2, song "b" 3])

movingOn :: Spec
movingOn = describe "next" $ do
  it "plays the next song of the album" $ withStandin $ \standin session -> do
    picking session tracks first
    next session
    current session `shouldReturn` Just second
    loaded standin `shouldReturn` fmap from [first, second]

  it "ends the playing on the last song" $ withStandin $ \standin session -> do
    picking session tracks fourth
    next session
    nowPlaying session `shouldReturn` Nothing
    motionOf standin `shouldReturn` Nothing

  it "starts nothing more once it has ended the playing" $
    withStandin $ \standin session -> do
      picking session tracks fourth
      next session
      next session
      _ <- runOut standin session 3
      loaded standin `shouldReturn` [from fourth]

movingBack :: Spec
movingBack = describe "previous" $ do
  it "goes to the previous song even part-way through this one" $
    withStandin $ \standin session -> do
      picking session tracks third
      reach standin (Seconds 90)
      previous session
      current session `shouldReturn` Just second
      loaded standin `shouldReturn` [from third, from second]

  it "plays the first song again when it is on the first" $
    withStandin $ \standin session -> do
      picking session tracks first
      reach standin (Seconds 90)
      previous session
      current session `shouldReturn` Just first
      loaded standin `shouldReturn` [from first, from first]

unplayable :: Spec
unplayable = describe "a song that will not play" $ do
  it "is skipped: its failure is shown and the next song of the album starts" $
    withStandin $ \standin session -> do
      let failure = Unplayable "the file will not play: unrecognized file format"
      picking session tracks second
      breakWith standin failure
      attend session `shouldReturn` [failure]
      current session `shouldReturn` Just third
      loaded standin `shouldReturn` [from second, from third]

  it "ends the playing when it was the album's last song" $
    withStandin $ \standin session -> do
      let failure = Unplayable "the file will not play: unrecognized file format"
      picking session tracks fourth
      breakWith standin failure
      attend session `shouldReturn` [failure]
      nowPlaying session `shouldReturn` Nothing

unreachable :: Spec
unreachable = describe "a server that cannot be reached" $ do
  it "stops the playing and shows the failure" $ withStandin $ \standin session -> do
    let failure = Unreachable "the server could not be reached: loading failed"
    picking session tracks second
    breakWith standin failure
    attend session `shouldReturn` [failure]
    nowPlaying session `shouldReturn` Nothing
    motionOf standin `shouldReturn` Nothing

  it "starts no further song of the album" $ withStandin $ \standin session -> do
    picking session tracks second
    breakWith standin (Unreachable "the server could not be reached: loading failed")
    _ <- attend session
    _ <- runOut standin session 3
    loaded standin `shouldReturn` [from second]

held :: Spec
held = describe "pausing, resuming and seeking" $ do
  it "holds the audio, and leaves the song where it is" $
    withStandin $ \standin session -> do
      picking session tracks second
      reach standin (Seconds 60)
      pause session
      motionOf standin `shouldReturn` Just Paused
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 60) Picked Loading)
      loaded standin `shouldReturn` [from second]

  it "lets it run on from where it was held" $ withStandin $ \standin session -> do
    picking session tracks second
    reach standin (Seconds 60)
    pause session
    resume session
    motionOf standin `shouldReturn` Just Running
    nowPlaying session `shouldReturn` Just (Playing second (Seconds 60) Picked Loading)

  it "holds a running song and lets a held one run on, on the one control" $
    withStandin $ \standin session -> do
      picking session tracks second
      reach standin (Seconds 60)
      togglePause session
      motionOf standin `shouldReturn` Just Paused
      togglePause session
      motionOf standin `shouldReturn` Just Running
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 60) Picked Loading)

  it "holds nothing when there is nothing playing to hold" $
    withStandin $ \standin session -> do
      togglePause session
      motionOf standin `shouldReturn` Nothing
      nowPlaying session `shouldReturn` Nothing

  it "moves through the song without changing which song it is" $
    withStandin $ \standin session -> do
      picking session tracks second
      reach standin (Seconds 60)
      seekBy session 30
      nowPlaying session `shouldReturn` Just (Playing second (Seconds 90) Picked Loading)
      loaded standin `shouldReturn` [from second]

  it "leaves the rest of the album to play as it would have" $
    withStandin $ \standin session -> do
      picking session tracks second
      pause session
      resume session
      seekBy session 30
      _ <- runOut standin session 3
      loaded standin `shouldReturn` fmap from [second, third, fourth]

stopped :: Spec
stopped = describe "stopping" $
  it "plays nothing, and leaves no album behind to carry on" $
    withStandin $ \standin session -> do
      picking session tracks second
      stop session
      nowPlaying session `shouldReturn` Nothing
      motionOf standin `shouldReturn` Nothing
      _ <- runOut standin session 3
      loaded standin `shouldReturn` [from second]
