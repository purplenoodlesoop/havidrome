{- | The whole of a playback session, driven against a stand-in backend: no
audio device, no server, and nothing of the terminal.
-}
module Havidrome.PlaybackTest (tests) where

import Control.Monad (replicateM, replicateM_)
import Data.Foldable (traverse_)
import Data.Maybe (listToMaybe)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Audio.State (Failure (Unplayable, Unreachable), Motion (Paused, Running))
import Havidrome.Check (Checks, example)
import Havidrome.Playback
import Havidrome.Playback.Standin
import Havidrome.Subsonic.Types (Seconds (Seconds), Song (..), SongId (SongId))
import Hedgehog (Gen, Group (Group), PropertyT, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lists (drop1)

tests :: Group
tests =
  Group
    "Havidrome.Playback"
    ( picking
        <> asPicked
        <> followed
        <> replacing
        <> walking
        <> skips
        <> holding
        <> seeks
        <> anyAlbum
        <> anyFailure
    )

-- | What picking a song plays, and what it leaves playing.
picking :: Checks
picking =
  [
    ( "picking a song plays that song, from its beginning"
    , example do
        told <- driving $ \standin session -> do
          startAt session tracks 1
          loaded standin
        told === [from (trackAt 1)]
    )
  ,
    ( "then plays the rest of its album, in album order, with nothing more asked"
    , example do
        (shown, told) <- driving $ \standin session -> do
          startAt session tracks 1
          shown <- runOut standin session 3
          (,) shown <$> loaded standin
        shown === []
        told === fmap from (drop1 tracks)
    )
  ,
    ( "plays nothing outside that album, however long it is left alone"
    , example do
        told <- driving $ \standin session -> do
          startAt session tracks 0
          _ <- runOut standin session 8
          loaded standin
        told === fmap from tracks
    )
  ,
    ( "leaves nothing playing once the album has run out"
    , example do
        (song, motion) <- driving $ \standin session -> do
          startAt session tracks 2
          _ <- runOut standin session 2
          (,) <$> session.nowPlaying <*> motionOf standin
        (fmap (.song) song, motion) === (Nothing, Nothing)
    )
  ]

-- | The picked song a session reports, and how far it has loaded.
asPicked :: Checks
asPicked =
  [
    ( "is picked for a song the album was started from, loading until its audio starts"
    , example do
        (loading, sounding) <- driving $ \standin session -> do
          startAt session tracks 1
          loading <- session.nowPlaying
          begin standin
          (,) loading <$> session.nowPlaying
        loading === Just (Playing (trackAt 1) (Seconds 0) Picked Loading)
        sounding === Just (Playing (trackAt 1) (Seconds 0) Picked (Sounding Running))
    )
  ,
    ( "is picked, and loading, for a song picked while another still loads"
    , example do
        now <- driving $ \_ session -> do
          startAt session tracks 1
          startAt session tracks 2
          session.nowPlaying
        now === Just (Playing (trackAt 2) (Seconds 0) Picked Loading)
    )
  ,
    ( "is followed for the song the album runs on to"
    , example do
        now <- driving $ \standin session -> do
          startAt session tracks 1
          begin standin
          _ <- runOut standin session 1
          session.nowPlaying
        now === Just (Playing (trackAt 2) (Seconds 0) Followed Loading)
    )
  ]

-- | The song the album runs on to, and a picked one held while it loads.
followed :: Checks
followed =
  [
    ( "is followed for the songs next and previous move to"
    , example do
        (onward, back) <- driving $ \_ session -> do
          startAt session tracks 1
          session.next
          onward <- session.nowPlaying
          session.previous
          (,) onward <$> session.nowPlaying
        onward === Just (Playing (trackAt 2) (Seconds 0) Followed Loading)
        back === Just (Playing (trackAt 1) (Seconds 0) Followed Loading)
    )
  ,
    ( "is followed for the song a skipped one gives way to"
    , example do
        arrival <- driving $ \standin session -> do
          startAt session tracks 1
          breakWith standin (Unplayable "the file will not play: unrecognized file format")
          _ <- session.attend
          fmap (.arrival) <$> session.nowPlaying
        arrival === Just Followed
    )
  ,
    ( "leaves a picked song held while it loads held at its start once loaded"
    , example do
        (now, motion) <- driving $ \standin session -> do
          startAt session tracks 1
          session.togglePause
          begin standin
          (,) <$> session.nowPlaying <*> motionOf standin
        now === Just (Playing (trackAt 1) (Seconds 0) Picked (Sounding Paused))
        motion === Just Paused
    )
  ]

-- | A song picked out of another album, which replaces the first.
replacing :: Checks
replacing =
  [
    ( "picking a song of another album replaces what is playing, and goes on through it"
    , example do
        (song, told) <- driving $ \standin session -> do
          let other = album "b" 3
          startAt session tracks 0
          startAt session other 1
          _ <- runOut standin session 2
          (,) <$> current session <*> loaded standin
        song === Nothing
        told === from (trackAt 0) : fmap from (drop1 (album "b" 3))
    )
  ]

-- | next and previous, which walk the album a song at a time.
walking :: Checks
walking =
  [
    ( "next plays the next song of the album"
    , example do
        (song, told) <- driving $ \standin session -> do
          startAt session tracks 0
          session.next
          (,) <$> current session <*> loaded standin
        song === Just (trackAt 1)
        told === fmap from (take 2 tracks)
    )
  ,
    ( "next ends the playing on the last song"
    , example do
        (song, motion) <- driving $ \standin session -> do
          startAt session tracks 3
          session.next
          (,) <$> current session <*> motionOf standin
        (song, motion) === (Nothing, Nothing)
    )
  ,
    ( "next starts nothing more once it has ended the playing"
    , example do
        told <- driving $ \standin session -> do
          startAt session tracks 3
          session.next
          session.next
          _ <- runOut standin session 3
          loaded standin
        told === [from (trackAt 3)]
    )
  ,
    ( "previous goes to the previous song even part-way through this one"
    , example do
        (song, told) <- driving $ \standin session -> do
          startAt session tracks 2
          reach standin (Seconds 90)
          session.previous
          (,) <$> current session <*> loaded standin
        song === Just (trackAt 1)
        told === [from (trackAt 2), from (trackAt 1)]
    )
  ,
    ( "previous plays the first song again when it is on the first"
    , example do
        (song, told) <- driving $ \standin session -> do
          startAt session tracks 0
          reach standin (Seconds 90)
          session.previous
          (,) <$> current session <*> loaded standin
        song === Just (trackAt 0)
        told === [from (trackAt 0), from (trackAt 0)]
    )
  ]

-- | A song that will not play, and a server that cannot be reached.
skips :: Checks
skips =
  [
    ( "a song that will not play is skipped: its failure is shown and the next one starts"
    , example do
        let failure = Unplayable "the file will not play: unrecognized file format"
        (shown, song, told) <- driving $ \standin session -> do
          startAt session tracks 1
          breakWith standin failure
          shown <- session.attend
          (,,) shown <$> current session <*> loaded standin
        shown === [failure]
        song === Just (trackAt 2)
        told === [from (trackAt 1), from (trackAt 2)]
    )
  ,
    ( "a song that will not play ends the playing when it was the album's last"
    , example do
        let failure = Unplayable "the file will not play: unrecognized file format"
        (shown, song) <- driving $ \standin session -> do
          startAt session tracks 3
          breakWith standin failure
          shown <- session.attend
          (,) shown <$> current session
        shown === [failure]
        song === Nothing
    )
  ,
    ( "a server that cannot be reached stops the playing and shows the failure"
    , example do
        let failure = Unreachable "the server could not be reached: loading failed"
        (shown, song, motion) <- driving $ \standin session -> do
          startAt session tracks 1
          breakWith standin failure
          shown <- session.attend
          (,,) shown <$> current session <*> motionOf standin
        shown === [failure]
        (song, motion) === (Nothing, Nothing)
    )
  ,
    ( "a server that cannot be reached starts no further song of the album"
    , example do
        told <- driving $ \standin session -> do
          startAt session tracks 1
          breakWith standin (Unreachable "the server could not be reached: loading failed")
          _ <- session.attend
          _ <- runOut standin session 3
          loaded standin
        told === [from (trackAt 1)]
    )
  ]

-- | The pause, the resume, and the one control that does both.
holding :: Checks
holding =
  [
    ( "pausing holds the audio, and leaves the song where it is"
    , example do
        (motion, now, told) <- driving $ \standin session -> do
          startAt session tracks 1
          reach standin (Seconds 60)
          session.pause
          (,,) <$> motionOf standin <*> session.nowPlaying <*> loaded standin
        motion === Just Paused
        now === Just (Playing (trackAt 1) (Seconds 60) Picked Loading)
        told === [from (trackAt 1)]
    )
  ,
    ( "resuming lets it run on from where it was held"
    , example do
        (motion, now) <- driving $ \standin session -> do
          startAt session tracks 1
          reach standin (Seconds 60)
          session.pause
          session.resume
          (,) <$> motionOf standin <*> session.nowPlaying
        motion === Just Running
        now === Just (Playing (trackAt 1) (Seconds 60) Picked Loading)
    )
  ,
    ( "one control holds a running song and lets a held one run on"
    , example do
        (held, running, now) <- driving $ \standin session -> do
          startAt session tracks 1
          reach standin (Seconds 60)
          session.togglePause
          held <- motionOf standin
          session.togglePause
          (,,) held <$> motionOf standin <*> session.nowPlaying
        held === Just Paused
        running === Just Running
        now === Just (Playing (trackAt 1) (Seconds 60) Picked Loading)
    )
  ,
    ( "it holds nothing when there is nothing playing to hold"
    , example do
        (motion, now) <- driving $ \standin session -> do
          session.togglePause
          (,) <$> motionOf standin <*> session.nowPlaying
        (motion, fmap (.song) now) === (Nothing, Nothing)
    )
  ]

-- | A seek, which moves through the song and not out of it.
seeks :: Checks
seeks =
  [
    ( "seeking moves through the song without changing which song it is"
    , example do
        (now, told) <- driving $ \standin session -> do
          startAt session tracks 1
          reach standin (Seconds 60)
          session.seekBy 30
          (,) <$> session.nowPlaying <*> loaded standin
        now === Just (Playing (trackAt 1) (Seconds 90) Picked Loading)
        told === [from (trackAt 1)]
    )
  ,
    ( "the controls leave the rest of the album to play as it would have"
    , example do
        told <- driving $ \standin session -> do
          startAt session tracks 1
          session.pause
          session.resume
          session.seekBy 30
          _ <- runOut standin session 3
          loaded standin
        told === fmap from (drop1 tracks)
    )
  ,
    ( "stopping plays nothing, and leaves no album behind to carry on"
    , example do
        (song, motion, told) <- driving $ \standin session -> do
          startAt session tracks 1
          session.stop
          song <- current session
          motion <- motionOf standin
          _ <- runOut standin session 3
          (,,) song motion <$> loaded standin
        (song, motion) === (Nothing, Nothing)
        told === [from (trackAt 1)]
    )
  ]

-- | What holds over an album of any length, started anywhere in it.
anyAlbum :: Checks
anyAlbum =
  [
    ( "plays the song it was started from and then the rest of its album, whatever album"
    , property do
        (picked, place) <- forAll anAlbum
        told <- driving $ \standin session -> do
          startAt session picked place
          _ <- runOut standin session (length picked - place)
          loaded standin
        told === fmap from (drop place picked)
    )
  ,
    ( "plays nothing at all outside that album, however long it is left alone"
    , property do
        (picked, place) <- forAll anAlbum
        spare <- forAll (Gen.int (Range.linear 0 6))
        told <- driving $ \standin session -> do
          startAt session picked place
          _ <- runOut standin session (length picked - place + spare)
          loaded standin
        told === fmap from (drop place picked)
    )
  ,
    ( "next walks forward through the album and ends the playing past its last song"
    , property do
        (picked, place) <- forAll anAlbum
        presses <- forAll (Gen.int (Range.linear 1 8))
        song <- driving $ \_ session -> do
          startAt session picked place
          replicateM_ presses session.next
          current session
        song === inPlace picked (place + presses)
    )
  ,
    ( "previous walks back through the album and holds at its first song"
    , property do
        (picked, place) <- forAll anAlbum
        presses <- forAll (Gen.int (Range.linear 1 8))
        song <- driving $ \_ session -> do
          startAt session picked place
          replicateM_ presses session.previous
          current session
        song === inPlace picked (max 0 (place - presses))
    )
  ]

-- | What a failure does wherever in such an album it falls.
anyFailure :: Checks
anyFailure =
  [
    ( "a song that will not play gives way to the next of the album, wherever it fell"
    , property do
        (picked, place) <- forAll anAlbum
        let failure = Unplayable "the file will not play: it is corrupt"
        (shown, song) <- driving $ \standin session -> do
          startAt session picked place
          breakWith standin failure
          (,) <$> session.attend <*> current session
        shown === [failure]
        song === inPlace picked (place + 1)
    )
  ,
    ( "a server that cannot be reached stops the playing, wherever in the album it fell"
    , property do
        (picked, place) <- forAll anAlbum
        let failure = Unreachable "the server could not be reached: it is down"
        (shown, song, told) <- driving $ \standin session -> do
          startAt session picked place
          breakWith standin failure
          shown <- session.attend
          song <- current session
          _ <- runOut standin session 3
          (,,) shown song <$> loaded standin
        shown === [failure]
        song === Nothing
        told === fmap from (take 1 (drop place picked))
    )
  ]

{- | The album every example here is played out of: four songs, in album
order.
-}
tracks :: [Song]
tracks = album "a" 4

{- | An album of so many songs, in album order, told apart from another
album's by the name its songs are lettered with.
-}
album :: Text -> Int -> [Song]
album name count = fmap (numbered name) [1 .. count]

-- | The song of such an album with this number, counting from one.
numbered :: Text -> Int -> Song
numbered name n =
  Song
    { id = SongId (name <> T.pack (show n))
    , title = name <> T.pack (" track " <> show n)
    , duration = Seconds 180
    , track = Just n
    , disc = Nothing
    }

{- | The song in this place of 'tracks', counting from nothing. It is named
rather than looked up, so that there is no place the album does not hold.
-}
trackAt :: Int -> Song
trackAt place = numbered "a" (place + 1)

{- | The song in this place of an album, and nothing where the album is
shorter than that.
-}
inPlace :: [Song] -> Int -> Maybe Song
inPlace album' place = listToMaybe (drop place album')

-- | An album of any length, and a song of it to start playing at.
anAlbum :: Gen ([Song], Int)
anAlbum = do
  count <- Gen.int (Range.linear 1 6)
  place <- Gen.int (Range.linear 0 (count - 1))
  pure (album "a" count, place)

{- | Starts a session on the song in this place of an album, counting from
nothing. A place the album has no song in starts nothing at all, which
leaves the session playing nothing for whatever asked for it.
-}
startAt :: Session -> [Song] -> Int -> IO ()
startAt session album' place =
  traverse_ session.start (startingAt album' . (.id) =<< inPlace album' place)

-- | What the backend is told when that song is played from its beginning.
from :: Song -> (Text, Seconds)
from song = (address song.id, Seconds 0)

{- | Nothing is asked of the session: the audio simply runs out, so many
times, and the session is handed what the backend says about it. This is
the whole of \"without further input\".
-}
runOut :: Standin -> Session -> Int -> IO [Failure]
runOut standin session times =
  fmap concat . replicateM (max 0 times) $ do
    finish standin
    session.attend

-- | The song a session is playing, if it is playing one.
current :: Session -> IO (Maybe Song)
current session = fmap (fmap (.song)) session.nowPlaying

-- | What a run against a stand-in backend, over a session of its own, came to.
driving :: (Standin -> Session -> IO a) -> PropertyT IO a
driving use = evalIO (withStandin use)
