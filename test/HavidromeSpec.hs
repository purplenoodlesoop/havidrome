-- | The player as a whole: where a run starts, the accounts it goes through
-- one after another, and the points where it has nowhere to browse.
--
-- The login screen, the browsing screen and the config file here are a
-- stand-in that answers from a script and writes down what it was asked, so a
-- spec sees exactly what a run did and in what order — a logout among it.
module HavidromeSpec (spec) where

import Control.Exception (bracket, finally)
import Control.Monad.Trans.State.Strict (State, execState, state)
import Data.Text (Text)
import GHC.IO.Handle (hDuplicate, hDuplicateTo)
import Havidrome
  ( Account (Account, asks, browses, forgets)
  , Start (Ask, Browse, Stop)
  , player
  , run
  , start
  )
import Havidrome.Browse.Screen (Ending (LoggedOut, Quit))
import Havidrome.Credentials
  ( Credentials (Credentials)
  , Fault (MissingField)
  , Stored (Absent, Present, Unreadable)
  , save
  )
import System.Environment (setEnv, unsetEnv)
import System.Exit (ExitCode (ExitFailure))
import System.IO (IOMode (WriteMode), hClose, stderr, withFile)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy, shouldThrow)

spec :: Spec
spec = do
  describe "start" $ do
    it "asks for credentials when none are stored" $
      start Absent `shouldBe` Ask

    it "browses with the credentials that are stored, asking nothing" $
      start (Present someone) `shouldBe` Browse someone

    it "stops when what is stored cannot be read as credentials" $
      start (Unreadable (MissingField "password")) `shouldSatisfy` \started ->
        case started of
          Stop _ -> True
          _ -> False

  describe "player" $ do
    it "browses the account a run starts with, asking for none" $
      ran [] [Quit] (Just someone) `shouldBe` [Browsed someone]

    it "asks for an account when a run starts with none" $
      ran [Just someone] [Quit] Nothing `shouldBe` [Asked, Browsed someone]

    it "browses nothing when the login screen is left" $
      ran [Nothing] [] Nothing `shouldBe` [Asked]

    it "forgets the account on a logout, and browses whatever is entered next" $
      ran [Just anyone] [LoggedOut, Quit] (Just someone)
        `shouldBe` [Browsed someone, Forgot, Asked, Browsed anyone]

    it "goes on doing so, logout after logout" $
      ran [Just anyone, Just someone] [LoggedOut, LoggedOut, Quit] (Just someone)
        `shouldBe` [ Browsed someone
                   , Forgot
                   , Asked
                   , Browsed anyone
                   , Forgot
                   , Asked
                   , Browsed someone
                   ]

    it "has forgotten the account already when that login screen is left" $
      ran [Nothing] [LoggedOut] (Just someone)
        `shouldBe` [Browsed someone, Forgot, Asked]

  describe "run" $
    it "stops instead of browsing when the artist list cannot be fetched" $
      withConfigHome $ do
        save (Credentials nowhere "someone" "secret")
        quietly run `shouldThrow` (== ExitFailure 1)

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
nextAnswer = state $ \script -> case script.answers of
  [] -> (Nothing, script)
  (answer : rest) -> (answer, script {answers = rest})

-- | How browsing ends this time; with the endings used up, the player is left.
nextEnding :: State Script Ending
nextEnding = state $ \script -> case script.endings of
  [] -> (Quit, script)
  (ended : rest) -> (ended, script {endings = rest})

note :: Step -> State Script ()
note doing = state (\script -> ((), script {taken = doing : script.taken}))

-- | Everything a run did, in the order it did it: from the credentials it
-- started with, against a login screen that answers so and browsing that ends
-- so.
ran :: [Maybe Credentials] -> [Ending] -> Maybe Credentials -> [Step]
ran answered ended from =
  reverse (execState (player scripted from) (Script answered ended [])).taken

-- | An invented address nothing answers on, so that the artist list fails to
-- arrive the way it fails against a server that cannot be reached.
nowhere :: Text
nowhere = "http://nowhere.example"

-- | An empty config directory of its own, so that the specs never read the
-- credentials of whoever is running them.
withConfigHome :: IO a -> IO a
withConfigHome action =
  withSystemTempDirectory "havidrome-config" $ \home ->
    bracket (setEnv "XDG_CONFIG_HOME" home) (const (unsetEnv "XDG_CONFIG_HOME")) (const action)

-- | Runs something with its complaints sent nowhere, so that a spec about
-- stopping does not print the reason among the results.
quietly :: IO a -> IO a
quietly action =
  withFile "/dev/null" WriteMode $ \sink ->
    bracket (hDuplicate stderr) hClose $ \saved -> do
      hDuplicateTo sink stderr
      action `finally` hDuplicateTo saved stderr
