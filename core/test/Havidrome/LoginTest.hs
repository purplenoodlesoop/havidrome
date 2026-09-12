-- | The login form: what the keys do, and what a submit does with what was
-- typed.
--
-- The server and the config file here are a stand-in that writes down what it
-- was asked and what it was told to keep, so a test sees exactly what a submit
-- did — and, after a refusal, that it did nothing.
module Havidrome.LoginTest (tests) where

import Control.Monad (foldM)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
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
import Hedgehog (Group (Group), (===))

tests :: Group
tests = Group "Havidrome.Login" (keys <> filling <> submitting <> leaving <> reading)

-- | What each key press asks the screen for.
keys :: Checks
keys =
  [
    ( "the keys move to the next field on Tab and on the down arrow"
    , example do
        command (Character '\t') [] === Just Ahead
        command DownArrow [] === Just Ahead
    )
  ,
    ( "the keys move to the field before on the up arrow"
    , example (command UpArrow [] === Just Back)
    )
  ,
    ( "the keys submit on Enter"
    , example (command Enter [] === Just Submit)
    )
  ,
    ( "the keys leave the player on Ctrl+C"
    , example (command (Character 'c') [Ctrl] === Just Leave)
    )
  ,
    ( "the keys type a printable character into the field"
    , example do
        command (Character 'x') [] === Just (Type 'x')
        command (Character ' ') [] === Just (Type ' ')
        command (Character '!') [] === Just (Type '!')
    )
  ,
    ( "the keys type q like any other character, quitting from nowhere"
    , example (command (Character 'q') [] === Just (Type 'q'))
    )
  ,
    ( "the keys take a character back on Backspace"
    , example (command Backspace [] === Just Rub)
    )
  ,
    ( "the keys ignore every other key"
    , example do
        command Escape [] === Nothing
        command LeftArrow [] === Nothing
        command (Character 'q') [Ctrl] === Nothing
    )
  ]

-- | The three fields, moved between and typed into.
filling :: Checks
filling =
  [
    ( "the three fields open on the server URL, all three of them empty"
    , example do
        blank.focus === ServerUrl
        fmap (`value` blank) fields === ["", "", ""]
    )
  ,
    ( "the three fields are moved through in the order they are asked in"
    , example do
        fmap ahead fields === [Username, Password, ServerUrl]
        fmap back fields === [Password, ServerUrl, Username]
    )
  ,
    ( "moving forward from the password lands on the server URL"
    , example (fmap (.focus) (typing [Ahead, Ahead, Ahead]) === Right ServerUrl)
    )
  ,
    ( "moving back from the server URL lands on the password"
    , example (fmap (.focus) (typing [Back]) === Right Password)
    )
  ,
    ( "typing goes into the focused field and no other"
    , example (filledIn (typing (typed "me")) === Right ["me", "", ""])
    )
  ,
    ( "typing goes into whichever field was moved to"
    , example (filledIn (typing (typedInto Username "me")) === Right ["", "me", ""])
    )
  ,
    ( "a rub takes back the last character of the focused field"
    , example (filledIn (typing (typed "mee" <> [Rub])) === Right ["me", "", ""])
    )
  ,
    ( "a rub takes nothing back from a field that is empty"
    , example (filledIn (typing [Rub, Rub]) === Right ["", "", ""])
    )
  ]

-- | Handing the details to the server, and what comes back.
submitting :: Checks
submitting =
  [
    ( "submitting hands back the credentials a server took"
    , example (fst (answering (Right ()) details) === Left (Entered someone))
    )
  ,
    ( "submitting stores them, so that a later run has them"
    , example (snd (answering (Right ()) details) === Log [someone] [someone])
    )
  ,
    ( "submitting works from any of the three fields"
    , example (fst (answering (Right ()) (filled <> [Back, Submit])) === Left (Entered someone))
    )
  ,
    ( "submitting stays on the screen when the server refuses the credentials"
    , example
        ( fmap (.trouble) (fst (answering refusal details))
            === Right (Just "The server refused these credentials: wrong password")
        )
    )
  ,
    ( "submitting stays on the screen when the server cannot be reached"
    , example
        ( fmap (.trouble) (fst (answering unreachable details))
            === Right (Just "The server could not be reached: no route to host")
        )
    )
  ,
    ( "submitting stores nothing that was not accepted"
    , example do
        (snd (answering refusal details)).kept === []
        (snd (answering unreachable details)).kept === []
    )
  ,
    ( "submitting keeps what was typed, for it to be typed over"
    , example do
        filledIn (fst (answering refusal details))
          === Right ["https://music.example.org", "someone", "secret"]
        filledIn (fst (answering refusal (details <> [Rub, Type 't'])))
          === Right ["https://music.example.org", "someone", "secret"]
    )
  ,
    ( "submitting asks the server once, and does nothing again of its own accord"
    , example (asks (snd (answering refusal details)) === 1)
    )
  ,
    ( "submitting asks it again only when it is submitted again"
    , example (asks (snd (answering refusal (details <> [Submit]))) === 2)
    )
  ,
    ( "a key press clears what the server was last heard to say"
    , example (fmap (.trouble) (fst (answering refusal (details <> [Type 'x']))) === Right Nothing)
    )
  ]

-- | Ending the screen without logging in.
leaving :: Checks
leaving =
  [
    ( "leaving ends the screen on Ctrl+C"
    , example (fst (answering (Right ()) [Leave]) === Left Abandoned)
    )
  ,
    ( "leaving stores nothing on the way out"
    , example (snd (answering (Right ()) (filled <> [Leave])) === Log [] [])
    )
  ,
    ( "leaving happens from any field, and with a server's refusal on screen"
    , example do
        fst (answering (Right ()) [Ahead, Ahead, Leave]) === Left Abandoned
        fst (answering refusal (details <> [Leave])) === Left Abandoned
    )
  ]

-- | How the screen reads while it is being filled in.
reading :: Checks
reading =
  [
    ( "a field masks the password and nothing else"
    , example do
        masked Password "secret" === "••••••"
        masked ServerUrl "secret" === "secret"
        masked Username "secret" === "secret"
    )
  ,
    ( "the three labels stand in a column of their own"
    , example (fmap labelled fields === ["Server URL  ", "Username    ", "Password    "])
    )
  ]

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
filledIn = fmap (\form -> fmap (`value` form) fields)

-- | Typing something into the field the form is on.
typed :: Text -> [Command]
typed = fmap Type . T.unpack

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
