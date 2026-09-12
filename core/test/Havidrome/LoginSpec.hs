-- | The login form: what the keys do, and what a submit does with what was
-- typed.
--
-- The server and the config file here are a stand-in that writes down what it
-- was asked and what it was told to keep, so a spec sees exactly what a submit
-- did — and, after a refusal, that it did nothing.
module Havidrome.LoginSpec (spec) where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Credentials (Credentials (Credentials))
import Havidrome.Key (Key (..), Modifier (Ctrl))
import Havidrome.Login
  ( Command (Ahead, Back, Leave, Rub, Submit, Type)
  , Ending (Abandoned, Entered)
  , Entry (Entry, accepts, keeps)
  , Field (Password, ServerUrl, Username)
  , Form (focus, trouble)
  , ahead
  , back
  , blank
  , command
  , labelled
  , masked
  , step
  , value
  )
import Havidrome.Subsonic.Types (SubsonicError (AuthRejected, NetworkFailure))
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = do
  describe "command" $ do
    it "moves to the next field on Tab and on the down arrow" $ do
      command (Character '\t') [] `shouldBe` Just Ahead
      command DownArrow [] `shouldBe` Just Ahead

    it "moves to the field before on the up arrow" $
      command UpArrow [] `shouldBe` Just Back

    it "submits on Enter" $
      command Enter [] `shouldBe` Just Submit

    it "leaves the player on Ctrl+C" $
      command (Character 'c') [Ctrl] `shouldBe` Just Leave

    it "types a printable character into the field" $ do
      command (Character 'x') [] `shouldBe` Just (Type 'x')
      command (Character ' ') [] `shouldBe` Just (Type ' ')
      command (Character '!') [] `shouldBe` Just (Type '!')

    it "types q like any other character, quitting from nowhere" $
      command (Character 'q') [] `shouldBe` Just (Type 'q')

    it "takes a character back on Backspace" $
      command Backspace [] `shouldBe` Just Rub

    it "ignores every other key" $ do
      command Escape [] `shouldBe` Nothing
      command LeftArrow [] `shouldBe` Nothing
      command (Character 'q') [Ctrl] `shouldBe` Nothing

  describe "the three fields" $ do
    it "opens on the server URL, all three of them empty" $ do
      blank.focus `shouldBe` ServerUrl
      map (`value` blank) fields `shouldBe` ["", "", ""]

    it "moves through them in the order they are asked in" $ do
      map ahead fields `shouldBe` [Username, Password, ServerUrl]
      map back fields `shouldBe` [Password, ServerUrl, Username]

    it "lands on the server URL moving forward from the password" $
      (.focus) <$> typing [Ahead, Ahead, Ahead] `shouldBe` Right ServerUrl

    it "lands on the password moving back from the server URL" $
      (.focus) <$> typing [Back] `shouldBe` Right Password

    it "types into the focused field and no other" $
      filledIn (typing (typed "me")) `shouldBe` Right ["me", "", ""]

    it "types into whichever field was moved to" $
      filledIn (typing (typedInto Username "me")) `shouldBe` Right ["", "me", ""]

    it "takes back the last character of the focused field" $
      filledIn (typing (typed "mee" <> [Rub])) `shouldBe` Right ["me", "", ""]

    it "takes nothing back from a field that is empty" $
      filledIn (typing [Rub, Rub]) `shouldBe` Right ["", "", ""]

  describe "submitting" $ do
    it "hands back the credentials a server took" $
      fst (answering (Right ()) details) `shouldBe` Left (Entered someone)

    it "stores them, so that a later run has them" $
      snd (answering (Right ()) details) `shouldBe` Log [someone] [someone]

    it "submits from any of the three fields" $
      fst (answering (Right ()) (filled <> [Back, Submit]))
        `shouldBe` Left (Entered someone)

    it "stays on the screen when the server refuses the credentials" $
      (.trouble) <$> fst (answering refusal details)
        `shouldBe` Right (Just "The server refused these credentials: wrong password")

    it "stays on the screen when the server cannot be reached" $
      (.trouble) <$> fst (answering unreachable details)
        `shouldBe` Right (Just "The server could not be reached: no route to host")

    it "stores nothing that was not accepted" $ do
      (snd (answering refusal details)).kept `shouldBe` []
      (snd (answering unreachable details)).kept `shouldBe` []

    it "keeps what was typed, for it to be typed over" $ do
      filledIn (fst (answering refusal details))
        `shouldBe` Right ["https://music.example.org", "someone", "secret"]
      filledIn (fst (answering refusal (details <> [Rub, Type 't'])))
        `shouldBe` Right ["https://music.example.org", "someone", "secret"]

    it "asks the server once, and does nothing again of its own accord" $
      asks (snd (answering refusal details)) `shouldBe` 1

    it "asks it again only when it is submitted again" $
      asks (snd (answering refusal (details <> [Submit]))) `shouldBe` 2

    it "clears what it was told on the next key press" $
      (.trouble) <$> fst (answering refusal (details <> [Type 'x']))
        `shouldBe` Right Nothing

  describe "leaving" $ do
    it "ends the screen on Ctrl+C" $
      fst (answering (Right ()) [Leave]) `shouldBe` Left Abandoned

    it "stores nothing on the way out" $
      snd (answering (Right ()) (filled <> [Leave])) `shouldBe` Log [] []

    it "leaves from any field, and with a server's refusal on screen" $ do
      fst (answering (Right ()) [Ahead, Ahead, Leave]) `shouldBe` Left Abandoned
      fst (answering refusal (details <> [Leave])) `shouldBe` Left Abandoned

  describe "what a field reads as" $ do
    it "masks the password and nothing else" $ do
      masked Password "secret" `shouldBe` "••••••"
      masked ServerUrl "secret" `shouldBe` "secret"
      masked Username "secret" `shouldBe` "secret"

    it "lines the three labels up in a column of their own" $
      map (Text.length . labelled) fields `shouldBe` [12, 12, 12]

-- | The fields, in the order they are asked in.
fields :: [Field]
fields = [ServerUrl, Username, Password]

-- | What a stand-in server was asked, and what it was told to keep.
data Log = Log
  { asked :: [Credentials]
  , kept :: [Credentials]
  }
  deriving stock (Eq, Show)

-- | How many times it was asked anything.
asks :: Log -> Int
asks seen = length seen.asked

-- | A stand-in for a server and the config file: it gives the same answer to
-- every check, and writes down everything that passes through it.
standin :: Either SubsonicError () -> Entry (State Log)
standin answer =
  Entry
    { accepts = \credentials -> do
        modify' (\seen -> seen {asked = seen.asked <> [credentials]})
        pure answer
    , keeps = \credentials -> modify' (\seen -> seen {kept = seen.kept <> [credentials]})
    }

-- | What these commands leave behind over a server giving that answer, and
-- what the server saw. A command that ends the screen leaves no form, and
-- nothing after it is taken.
answering :: Either SubsonicError () -> [Command] -> (Either Ending Form, Log)
answering answer instructions =
  runState (foldM taking (Right blank) instructions) (Log [] [])
  where
    taking (Right form) instruction = step (standin answer) instruction form
    taking ended _ = pure ended

-- | The same over a server that takes anything, for the commands that never
-- reach one.
typing :: [Command] -> Either Ending Form
typing = fst . answering (Right ())

-- | What stands in the three fields.
filledIn :: Either Ending Form -> Either Ending [Text]
filledIn = fmap (\form -> map (`value` form) fields)

-- | Typing something into the field the form is on.
typed :: Text -> [Command]
typed = map Type . Text.unpack

-- | From the field the screen opens on, moving to a field and typing into it.
typedInto :: Field -> Text -> [Command]
typedInto field written = replicate steps Ahead <> typed written
  where
    steps = length (takeWhile (/= field) fields)

-- | A whole account filled in, the typing left in the last field.
filled :: [Command]
filled =
  typed "https://music.example.org"
    <> [Ahead]
    <> typed "someone"
    <> [Ahead]
    <> typed "secret"

-- | The same, submitted.
details :: [Command]
details = filled <> [Submit]

-- | The account those details name.
someone :: Credentials
someone = Credentials "https://music.example.org" "someone" "secret"

refusal, unreachable :: Either SubsonicError ()
refusal = Left (AuthRejected "wrong password")
unreachable = Left (NetworkFailure "no route to host")
