{- | The player as a whole: where a run starts, the accounts it goes through
one after another, and the points where it has nowhere to browse.

The login screen, the browsing screen and the config file here are a
stand-in that answers from a script and writes down what it was asked, so a
test sees exactly what a run did and in what order — a logout among it.
-}
module HavidromeTest (tests) where

import Control.Exception (bracket, bracket_, finally, try)
import Control.Monad.Trans.State.Strict (State, execState, state)
import Data.ByteString qualified as ByteString
import Data.Text as T (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Havidrome
  ( Account (Account, asks, browses, forgets)
  , Start (Ask, Browse)
  , checked
  , player
  , run
  , start
  )
import Havidrome.Browse.Screen (Ending (LoggedOut, Quit))
import Havidrome.Check (Checks, example)
import Havidrome.Credentials (Credentials (Credentials), Fault (MissingField))
import Havidrome.Credentials.Store
  ( Store (file, load, save)
  , Stored (Absent, Present, Unreadable)
  , mkStore
  )
import Havidrome.Journal.Fake (silent)
import Havidrome.Subsonic
  ( SubsonicError (AuthRejected, MalformedResponse, NetworkFailure, ServerFailure)
  )
import Hedgehog (Gen, Group (Group), annotateShow, assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Lists (drop1)
import System.Directory (createDirectoryIfMissing)
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure))
import System.FilePath (takeDirectory, (</>))
import System.IO (IOMode (WriteMode), hClose, hFlush, stderr, withFile)
import System.IO.Temp (withSystemTempDirectory)

tests :: Group
tests =
  Group
    "Havidrome"
    ( starting
        <> checking
        <> opening
        <> logouts
        <> always
        <> stopping
    )

-- | Where a run starts, given what the config file holds.
starting :: Checks
starting =
  [
    ( "start asks for credentials when none are stored, with nothing said yet"
    , example (start Absent === Right (Ask Nothing))
    )
  ,
    ( "start takes up the credentials that are stored"
    , example (start (Present someone) === Right (Browse someone))
    )
  ,
    ( "start stops when what is stored cannot be read as credentials"
    , example do
        start (Unreadable (MissingField "password"))
          === Left "the stored credentials could not be read: MissingField \"password\""
    )
  ]

{- | What the server's answer about the stored credentials makes of a run: a
refusal is a login screen, and anything else in the way is a stop.
-}
checking :: Checks
checking =
  [
    ( "credentials the server takes are the ones a run browses with"
    , example (checked someone (Right ()) === Right (Browse someone))
    )
  ,
    ( "credentials the server refuses put up the login screen, carrying the refusal"
    , example do
        checked someone (Left (AuthRejected "Wrong username or password"))
          === Right
            (Ask (Just "The server refused these credentials: Wrong username or password"))
    )
  ,
    ( "a server that cannot be reached stops the run instead"
    , example do
        checked someone (Left (NetworkFailure "no route to host"))
          === Left "The server could not be reached: no route to host"
    )
  ,
    ( "a server that answers with anything else stops the run too"
    , example do
        checked someone (Left (ServerFailure 500 "sorry"))
          === Left "The server answered with an error (500): sorry"
        checked someone (Left (MalformedResponse "not JSON"))
          === Left "The server's answer could not be read: not JSON"
    )
  ]

-- | The account a run opens with, and the login screen it asks.
opening :: Checks
opening =
  [
    ( "browses the account a run starts with, asking for none"
    , example (ran [] [Quit] (Browse someone) === [Browsed someone])
    )
  ,
    ( "asks for an account when a run starts with none"
    , example do
        ran [Just someone] [Quit] (Ask Nothing) === [Asked Nothing, Browsed someone]
    )
  ,
    ( "browses nothing when the login screen is left"
    , example (ran [Nothing] [] (Ask Nothing) === [Asked Nothing])
    )
  ,
    ( "opens the login screen on the refusal, and browses what is entered there"
    , example do
        ran [Just anyone] [Quit] (Ask (Just refusal))
          === [Asked (Just refusal), Browsed anyone]
    )
  ,
    ( "leaves the refused credentials stored when that login screen is left"
    , example (ran [Nothing] [] (Ask (Just refusal)) === [Asked (Just refusal)])
    )
  ,
    ( "asks the screens after that one with nothing said"
    , example do
        ran [Just anyone, Just someone] [LoggedOut, Quit] (Ask (Just refusal))
          === [ Asked (Just refusal)
              , Browsed anyone
              , Forgot
              , Asked Nothing
              , Browsed someone
              ]
    )
  ]

-- | A logout, and the login screen that follows it.
logouts :: Checks
logouts =
  [
    ( "forgets the account on a logout, and browses whatever is entered next"
    , example do
        ran [Just anyone] [LoggedOut, Quit] (Browse someone)
          === [Browsed someone, Forgot, Asked Nothing, Browsed anyone]
    )
  ,
    ( "goes on doing so, logout after logout"
    , example do
        ran [Just anyone, Just someone] [LoggedOut, LoggedOut, Quit] (Browse someone)
          === [ Browsed someone
              , Forgot
              , Asked Nothing
              , Browsed anyone
              , Forgot
              , Asked Nothing
              , Browsed someone
              ]
    )
  ,
    ( "has forgotten the account already when that login screen is left"
    , example do
        ran [Nothing] [LoggedOut] (Browse someone)
          === [Browsed someone, Forgot, Asked Nothing]
    )
  ]

-- | What holds of a run however the login screen answers it.
always :: Checks
always =
  [
    ( "forgets the account before every login screen but the first"
    , property do
        taken <- forAll script
        annotateShow taken.steps
        assert (all forgotten (following taken.steps))
    )
  ,
    ( "forgets only on the way back to a login screen, and every time"
    , property do
        taken <- forAll script
        annotateShow taken.steps
        assert (all asking (following taken.steps))
    )
  ,
    ( "browses the accounts it was handed, in the order it was handed them"
    , property do
        taken <- forAll script
        annotateShow taken.steps
        let handed = opened taken.started <> takeWhileJust taken.answering
        assert (browsed taken.steps `isPrefixOfList` handed)
    )
  ,
    ( "never browses two accounts, nor asks twice, without the other between"
    , property do
        taken <- forAll script
        annotateShow taken.steps
        assert (all apart (following taken.steps))
    )
  ]

-- | Where the player has nowhere to browse, and says so.
stopping :: Checks
stopping =
  [
    ( "run stops instead of browsing when the stored credentials reach no server"
    , example do
        (_, stopped) <- evalIO . withOwnDirectories $ do
          (mkStore silent).save (Credentials nowhere "someone" "secret")
          complaining run
        stopped === Left (ExitFailure 1)
    )
  ,
    ( "run leaves the credentials it could not reach a server with stored"
    , example do
        let stored = Credentials nowhere "someone" "secret"
        found <- evalIO . withOwnDirectories $ do
          (mkStore silent).save stored
          _ <- complaining run
          (mkStore silent).load
        found === Present stored
    )
  ,
    ( "run says on the terminal why it cannot go on, and leaves that line behind"
    , example do
        (said, stopped) <- evalIO . withOwnDirectories $ do
          path <- (mkStore silent).file
          createDirectoryIfMissing True (takeDirectory path)
          ByteString.writeFile path "server=https://music.example.org\nusername=someone\n"
          complaining run
        said === "havidrome: the stored credentials could not be read: MissingField \"password\"\n"
        stopped === Left (ExitFailure 1)
    )
  ]

-- | An account to start a run with. Nothing answers at its server.
someone :: Credentials
someone = Credentials nowhere "someone" "secret"

-- | Another account, on another server: what a logout is followed by.
anyone :: Credentials
anyone = Credentials "http://elsewhere.example" "anyone" "hunter2"

{- | What a server has already said about the credentials a run found stored,
which is what the login screen it opens on carries.
-}
refusal :: Text
refusal = "The server refused these credentials: Wrong username or password"

-- | One thing a run did with an account, written down as it did it.
data Step
  = -- | The login screen was asked for an account, carrying this.
    Asked (Maybe Text)
  | -- | This account's library was browsed.
    Browsed Credentials
  | -- | The stored credentials were thrown away.
    Forgot
  deriving stock (Eq, Show)

{- | What a run is told, and what it has done so far: the answers the login
screen gives, one per ask, the endings browsing comes to, one per account,
and the steps taken, the last one first.
-}
data Script = Script
  { answers :: [Maybe Credentials]
  , endings :: [Ending]
  , taken :: [Step]
  }

{- | A login screen, a browsing screen and a config file made of that script.

A script that runs out ends the run rather than going round again: the login
screen is left, and browsing quits.
-}
scripted :: Account (State Script)
scripted =
  Account
    { asks = \said -> note (Asked said) >> nextAnswer
    , browses = \credentials -> note (Browsed credentials) >> nextEnding
    , forgets = note Forgot
    }

{- | What the login screen answers this time; with the answers used up, it is
left instead.
-}
nextAnswer :: State Script (Maybe Credentials)
nextAnswer = state $ \given -> case given.answers of
  [] -> (Nothing, given)
  (answer : rest) -> (answer, given{answers = rest})

-- | How browsing ends this time; with the endings used up, the player is left.
nextEnding :: State Script Ending
nextEnding = state $ \given -> case given.endings of
  [] -> (Quit, given)
  (ended : rest) -> (ended, given{endings = rest})

note :: Step -> State Script ()
note doing = state (\given -> ((), given{taken = doing : given.taken}))

{- | Everything a run did, in the order it did it: from the credentials it
started with, against a login screen that answers so and browsing that ends
so.
-}
ran :: [Maybe Credentials] -> [Ending] -> Start -> [Step]
ran answered ended from =
  reverse (execState (player scripted from) (Script answered ended [])).taken

{- | A run to be made: where it starts, what the login screen will answer, how
browsing will end each time, and the steps it took when it was made.
-}
data Run = Run
  { started :: Start
  , answering :: [Maybe Credentials]
  , steps :: [Step]
  }
  deriving stock (Show)

{- | Runs of every shape: started on an account, on a login screen and on one
carrying a refusal, answered with accounts and with a login screen left, and
browsed to as many logouts as the endings hold.
-}
script :: Gen Run
script = do
  from <- anyStart
  answers <- Gen.list (Range.linear 0 5) (Gen.maybe anAccount)
  endings <- Gen.list (Range.linear 0 5) (Gen.element [LoggedOut, Quit])
  pure Run{started = from, answering = answers, steps = ran answers endings from}

-- | Where a run might start: at either login screen, or on an account.
anyStart :: Gen Start
anyStart =
  Gen.choice
    [ pure (Ask Nothing)
    , pure (Ask (Just refusal))
    , Browse <$> anAccount
    ]

-- | The accounts a run might be handed, told apart by their username.
anAccount :: Gen Credentials
anAccount = do
  name <- Gen.text (Range.linear 1 6) Gen.alpha
  pure (Credentials "http://nowhere.example" name "secret")

-- | Each step of a run beside the one before it.
following :: [Step] -> [(Step, Step)]
following steps = zip steps (drop1 steps)

{- | Whether a login screen that comes after something has a forgetting before
it.
-}
forgotten :: (Step, Step) -> Bool
forgotten (before, after) = not (asked after) || before == Forgot

-- | Whether a forgetting is on the way to a login screen.
asking :: (Step, Step) -> Bool
asking (before, after) = before /= Forgot || asked after

-- | Whether two steps in a row are not the same kind of step twice.
apart :: (Step, Step) -> Bool
apart = \case
  (Asked _, Asked _) -> False
  (Browsed _, Browsed _) -> False
  _ -> True

-- | Whether a step is a login screen, whatever it carried.
asked :: Step -> Bool
asked = \case
  Asked _ -> True
  _ -> False

-- | The account a run opens on, when it opens on one at all.
opened :: Start -> [Credentials]
opened = \case
  Browse credentials -> [credentials]
  Ask _ -> []

-- | The accounts a run browsed, in the order it browsed them.
browsed :: [Step] -> [Credentials]
browsed steps = [credentials | Browsed credentials <- steps]

{- | The answers up to the first one that leaves the login screen, which is
where a run stops asking.
-}
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = \case
  (Just one : rest) -> one : takeWhileJust rest
  _ -> []

isPrefixOfList :: (Eq a) => [a] -> [a] -> Bool
isPrefixOfList these those = take (length these) those == these

{- | An invented address nothing answers on, so that the artist list fails to
arrive the way it fails against a server that cannot be reached.
-}
nowhere :: Text
nowhere = "http://nowhere.example"

{- | Empty config and state directories of its own, so that a run here never
reads the credentials of whoever is running it, nor writes a line into their
journal.
-}
withOwnDirectories :: IO a -> IO a
withOwnDirectories action =
  withSystemTempDirectory "havidrome-config" $ \config ->
    withSystemTempDirectory "havidrome-state" $ \state' ->
      withEnvironment "XDG_CONFIG_HOME" config $
        withEnvironment "XDG_STATE_HOME" state' action

{- | Runs an action with one environment variable set to this path, unset again
afterwards.
-}
withEnvironment :: Text -> FilePath -> IO a -> IO a
withEnvironment name value = bracket_ (setEnv named value) (unsetEnv named)
 where
  named = T.unpack name

{- | Runs something with what it leaves on the terminal caught rather than
printed: the line comes back to be read, and none of it lands among the
results.
-}
complaining :: IO a -> IO (Text, Either ExitCode a)
complaining action =
  withSystemTempDirectory "havidrome-terminal" $ \dir -> do
    let path = dir </> "said"
    ended <- withFile path WriteMode $ \sink ->
      bracket (hDuplicate stderr) hClose $ \saved -> do
        hDuplicateTo sink stderr
        try action `finally` (hFlush stderr >> hDuplicateTo saved stderr)
    said <- ByteString.readFile path
    pure (T.decodeUtf8Lenient said, ended)
