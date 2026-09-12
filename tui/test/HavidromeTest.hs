-- | The player as a whole: where a run starts, the accounts it goes through
-- one after another, and the points where it has nowhere to browse.
--
-- The login screen, the browsing screen and the config file here are a
-- stand-in that answers from a script and writes down what it was asked, so a
-- test sees exactly what a run did and in what order — a logout among it.
module HavidromeTest (tests) where

import Control.Exception (bracket, finally, try)
import Control.Monad.Trans.State.Strict (State, execState, state)
import Data.ByteString qualified as ByteString
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Havidrome
  ( Account (Account, asks, browses, forgets)
  , Start (Ask, Browse, Stop)
  , player
  , run
  , start
  )
import Havidrome.Browse.Screen (Ending (LoggedOut, Quit))
import Havidrome.Check (example)
import Havidrome.Credentials (Credentials (Credentials), Fault (MissingField))
import Havidrome.Credentials.Store (Store (file, save), Stored (Absent, Present, Unreadable), mkStore)
import Havidrome.Journal.Fake (silent)
import Hedgehog (Gen, Group (Group), annotateShow, assert, evalIO, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
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
    [
      ( "start asks for credentials when none are stored"
      , example (start Absent === Ask)
      )
    ,
      ( "start browses with the credentials that are stored, asking nothing"
      , example (start (Present someone) === Browse someone)
      )
    ,
      ( "start stops when what is stored cannot be read as credentials"
      , example do
          start (Unreadable (MissingField "password"))
            === Stop "the stored credentials could not be read: MissingField \"password\""
      )
    ,
      ( "browses the account a run starts with, asking for none"
      , example (ran [] [Quit] (Just someone) === [Browsed someone])
      )
    ,
      ( "asks for an account when a run starts with none"
      , example (ran [Just someone] [Quit] Nothing === [Asked, Browsed someone])
      )
    ,
      ( "browses nothing when the login screen is left"
      , example (ran [Nothing] [] Nothing === [Asked])
      )
    ,
      ( "forgets the account on a logout, and browses whatever is entered next"
      , example do
          ran [Just anyone] [LoggedOut, Quit] (Just someone)
            === [Browsed someone, Forgot, Asked, Browsed anyone]
      )
    ,
      ( "goes on doing so, logout after logout"
      , example do
          ran [Just anyone, Just someone] [LoggedOut, LoggedOut, Quit] (Just someone)
            === [ Browsed someone
                , Forgot
                , Asked
                , Browsed anyone
                , Forgot
                , Asked
                , Browsed someone
                ]
      )
    ,
      ( "has forgotten the account already when that login screen is left"
      , example do
          ran [Nothing] [LoggedOut] (Just someone) === [Browsed someone, Forgot, Asked]
      )
    ,
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
          let handed = maybe [] pure taken.started <> takeWhileJust taken.answering
          assert (browsed taken.steps `isPrefixOfList` handed)
      )
    ,
      ( "never browses two accounts, nor asks twice, without the other between"
      , property do
          taken <- forAll script
          annotateShow taken.steps
          assert (all apart (following taken.steps))
      )
    ,
      ( "run stops instead of browsing when the artist list cannot be fetched"
      , example do
          (_, stopped) <- evalIO . withOwnDirectories $ do
            (mkStore silent).save (Credentials nowhere "someone" "secret")
            complaining run
          stopped === Left (ExitFailure 1)
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

-- | One thing a run did with an account, written down as it did it.
data Step
  = -- | The login screen was asked for an account.
    Asked
  | -- | This account's library was browsed.
    Browsed Credentials
  | -- | The stored credentials were thrown away.
    Forgot
  deriving stock (Eq, Show)

-- | What a run is told, and what it has done so far: the answers the login
-- screen gives, one per ask, the endings browsing comes to, one per account,
-- and the steps taken, the last one first.
data Script = Script
  { answers :: [Maybe Credentials]
  , endings :: [Ending]
  , taken :: [Step]
  }

-- | A login screen, a browsing screen and a config file made of that script.
--
-- A script that runs out ends the run rather than going round again: the login
-- screen is left, and browsing quits.
scripted :: Account (State Script)
scripted =
  Account
    { asks = note Asked >> nextAnswer
    , browses = \credentials -> note (Browsed credentials) >> nextEnding
    , forgets = note Forgot
    }

-- | What the login screen answers this time; with the answers used up, it is
-- left instead.
nextAnswer :: State Script (Maybe Credentials)
nextAnswer = state $ \given -> case given.answers of
  [] -> (Nothing, given)
  (answer : rest) -> (answer, given {answers = rest})

-- | How browsing ends this time; with the endings used up, the player is left.
nextEnding :: State Script Ending
nextEnding = state $ \given -> case given.endings of
  [] -> (Quit, given)
  (ended : rest) -> (ended, given {endings = rest})

note :: Step -> State Script ()
note doing = state (\given -> ((), given {taken = doing : given.taken}))

-- | Everything a run did, in the order it did it: from the credentials it
-- started with, against a login screen that answers so and browsing that ends
-- so.
ran :: [Maybe Credentials] -> [Ending] -> Maybe Credentials -> [Step]
ran answered ended from =
  reverse (execState (player scripted from) (Script answered ended [])).taken

-- | A run to be made: what it starts with, what the login screen will answer,
-- how browsing will end each time, and the steps it took when it was made.
data Run = Run
  { started :: Maybe Credentials
  , answering :: [Maybe Credentials]
  , steps :: [Step]
  }
  deriving stock (Show)

-- | Runs of every shape: started with an account or without one, answered
-- with accounts and with a login screen left, and browsed to as many logouts
-- as the endings hold.
script :: Gen Run
script = do
  from <- Gen.maybe anAccount
  answers <- Gen.list (Range.linear 0 5) (Gen.maybe anAccount)
  endings <- Gen.list (Range.linear 0 5) (Gen.element [LoggedOut, Quit])
  pure Run {started = from, answering = answers, steps = ran answers endings from}

-- | The accounts a run might be handed, told apart by their username.
anAccount :: Gen Credentials
anAccount = do
  name <- Gen.text (Range.linear 1 6) Gen.alpha
  pure (Credentials "http://nowhere.example" name "secret")

-- | Each step of a run beside the one before it.
following :: [Step] -> [(Step, Step)]
following steps = zip steps (drop 1 steps)

-- | Whether a login screen that comes after something has a forgetting before
-- it.
forgotten :: (Step, Step) -> Bool
forgotten (before, after) = after /= Asked || before == Forgot

-- | Whether a forgetting is on the way to a login screen.
asking :: (Step, Step) -> Bool
asking (before, after) = before /= Forgot || after == Asked

-- | Whether two steps in a row are not the same kind of step twice.
apart :: (Step, Step) -> Bool
apart = \case
  (Asked, Asked) -> False
  (Browsed _, Browsed _) -> False
  _ -> True

-- | The accounts a run browsed, in the order it browsed them.
browsed :: [Step] -> [Credentials]
browsed steps = [credentials | Browsed credentials <- steps]

-- | The answers up to the first one that leaves the login screen, which is
-- where a run stops asking.
takeWhileJust :: [Maybe a] -> [a]
takeWhileJust = \case
  (Just one : rest) -> one : takeWhileJust rest
  _ -> []

isPrefixOfList :: (Eq a) => [a] -> [a] -> Bool
isPrefixOfList these those = take (length these) those == these

-- | An invented address nothing answers on, so that the artist list fails to
-- arrive the way it fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://nowhere.example"

-- | Empty config and state directories of its own, so that a run here never
-- reads the credentials of whoever is running it, nor writes a line into their
-- journal.
withOwnDirectories :: IO a -> IO a
withOwnDirectories action =
  withSystemTempDirectory "havidrome-config" $ \config ->
    withSystemTempDirectory "havidrome-state" $ \state' ->
      withEnvironment "XDG_CONFIG_HOME" config $
        withEnvironment "XDG_STATE_HOME" state' action

withEnvironment :: String -> String -> IO a -> IO a
withEnvironment name value action =
  bracket (setEnv name value) (const (unsetEnv name)) (const action)

-- | Runs something with what it leaves on the terminal caught rather than
-- printed: the line comes back to be read, and none of it lands among the
-- results.
complaining :: IO a -> IO (Text, Either ExitCode a)
complaining action =
  withSystemTempDirectory "havidrome-terminal" $ \dir -> do
    let path = dir </> "said"
    ended <- withFile path WriteMode $ \sink ->
      bracket (hDuplicate stderr) hClose $ \saved -> do
        hDuplicateTo sink stderr
        try action `finally` (hFlush stderr >> hDuplicateTo saved stderr)
    said <- ByteString.readFile path
    pure (Text.decodeUtf8Lenient said, ended)
