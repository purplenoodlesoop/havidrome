{- | The login form: the three details an account on a Navidrome server is
reached by, what a key press does to them, and what submitting them does.

This is what a run with nothing stored opens on, and what a logout comes
back to. It ends in exactly one of two ways: a server took the credentials,
which are then stored and handed back for browsing, or the player was left.
Nothing here ever tries again on its own — a server is asked once per
submit, and only a key press asks it anything.
-}
module Havidrome.Login
  ( -- * The form
    Form (..)
  , Field (..)
  , fields
  , blank
  , value
  , ahead
  , back

    -- * The keys
  , Command (..)
  , command

    -- * Submitting
  , Entry (..)
  , Ending (..)
  , step

    -- * What it reads as
  , labelled
  , masked
  ) where

import Data.Char (isPrint)
import Data.Maybe (fromMaybe)
import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Credentials qualified as Credentials
import Havidrome.Key (Key (..), Modifier (Ctrl))
import Havidrome.Subsonic.Types (SubsonicError, explain)

{- | One of the three details the screen asks for, in the order they are asked
in.
-}
data Field
  = ServerUrl
  | Username
  | Password
  deriving stock (Eq, Ord, Show, Enum, Bounded)

{- | The screen as it has been filled in: what stands in each field, which
field the typing goes into, and whatever the last submit was told, which
sits in the strip along the bottom until the next key press.
-}
data Form = Form
  { serverUrl :: Text
  , username :: Text
  , password :: Text
  , focus :: Field
  , trouble :: Maybe Text
  }
  deriving stock (Eq)

{- | Shows every field but the password, which is never among the characters
shown — not on the screen, and not in a line of output either.
-}
instance Show Form where
  showsPrec d form =
    showParen (d > 10) $
      showString "Form "
        . showsPrec 11 form.serverUrl
        . showString " "
        . showsPrec 11 form.username
        . showString " <password> "
        . showsPrec 11 form.focus
        . showString " "
        . showsPrec 11 form.trouble

{- | The form the screen opens on: three empty fields, the typing in the first
of them, nothing wrong yet.
-}
blank :: Form
blank =
  Form
    { serverUrl = ""
    , username = ""
    , password = ""
    , focus = minBound
    , trouble = Nothing
    }

-- | What stands in one field.
value :: Field -> Form -> Text
value field form = case field of
  ServerUrl -> form.serverUrl
  Username -> form.username
  Password -> form.password

-- | The same field with its contents changed.
alter :: Field -> (Text -> Text) -> Form -> Form
alter field change form = case field of
  ServerUrl -> form{serverUrl = change form.serverUrl}
  Username -> form{username = change form.username}
  Password -> form{password = change form.password}

-- | Every field, in the order the form asks for them.
fields :: [Field]
fields = [minBound ..]

-- | The field after this one, round from the last back to the first.
ahead :: Field -> Field
ahead = following fields

-- | The field before this one, round from the first back to the last.
back :: Field -> Field
back = following (reverse fields)

-- The field each one of an order is followed by, the last followed by the
-- first. An order that names every field answers for every field, so the
-- field asked about is never the one given back.
following :: [Field] -> Field -> Field
following order field = fromMaybe field (lookup field (zip order rotated))
 where
  rotated = case order of
    [] -> []
    first : rest -> rest <> [first]

-- | What a key press means. Nothing else on the login screen does anything.
data Command
  = -- | To the next field.
    Ahead
  | -- | To the field before.
    Back
  | -- | One character into the focused field.
    Type Char
  | -- | The last character of the focused field, taken back.
    Rub
  | -- | Try the filled-in details against the server.
    Submit
  | -- | Leave the player.
    Leave
  deriving stock (Eq, Show)

{- | The key map: Tab and the down arrow move to the next field, the up arrow
to the one before, Enter submits, Backspace takes a character back, and
Ctrl+C leaves the player. Every other printable character is typed into the
focused field — @q@ among them, which quits from nowhere in this player.
-}
command :: Key -> [Modifier] -> Maybe Command
command key modifiers = case (key, modifiers) of
  (Character 'c', [Ctrl]) -> Just Leave
  (Character '\t', []) -> Just Ahead
  (DownArrow, []) -> Just Ahead
  (UpArrow, []) -> Just Back
  (Enter, []) -> Just Submit
  (Backspace, []) -> Just Rub
  (Character character, []) | isPrint character -> Just (Type character)
  _ -> Nothing

{- | What submitting a filled-in form does with it: ask a server whether it
takes these credentials, and, when it does, keep them for later runs.

The player's server is asked over the network and the credentials it takes
are kept in the config file. Anything else that can answer and keep — a
spec's stand-in, say — is as good an entry as far as the form is concerned,
which is what the @f@ keeps open.
-}
data Entry f = Entry
  { accepts :: Credentials.Credentials -> f (Either SubsonicError ())
  -- ^ Whether the server these credentials name takes them.
  , keeps :: Credentials.Credentials -> f ()
  -- ^ Store them, so that later runs do not ask again.
  }

-- | How the login screen ended.
data Ending
  = -- | A server took these credentials, and they are stored.
    Entered Credentials.Credentials
  | -- | The player was left, with nothing stored.
    Abandoned
  deriving stock (Eq, Show)

{- | The form a command leaves behind, or the ending it brought about.

Submitting is the one command that asks a server anything, and it asks
exactly once. A refusal and a server that cannot be reached both leave the
form exactly as it was typed, ready to be typed over, with the reason in the
bottom strip and nothing stored; the screen then sits there until another
key is pressed.
-}
step :: (Monad f) => Entry f -> Command -> Form -> f (Either Ending Form)
step entry instruction form = case instruction of
  Leave -> pure (Left Abandoned)
  Ahead -> stay quiet{focus = ahead form.focus}
  Back -> stay quiet{focus = back form.focus}
  Type character -> stay (alter form.focus (<> T.singleton character) quiet)
  Rub -> stay (alter form.focus dropLast quiet)
  Submit ->
    entry.accepts filled >>= \case
      Right () -> entry.keeps filled >> pure (Left (Entered filled))
      Left failure -> stay quiet{trouble = Just (explain failure)}
 where
  -- Whatever a key press does, it first clears what the last one was told.
  quiet = form{trouble = Nothing}
  stay = pure . Right
  filled =
    Credentials.Credentials
      { Credentials.server = form.serverUrl
      , Credentials.username = form.username
      , Credentials.password = form.password
      }

dropLast :: Text -> Text
dropLast = T.dropEnd 1

{- | What a field is called, in a column of its own so that what is typed into
the three of them lines up.
-}
labelled :: Field -> Text
labelled =
  T.justifyLeft 12 ' ' . \case
    ServerUrl -> "Server URL"
    Username -> "Username"
    Password -> "Password"

{- | What a field's contents read as on screen. Two of them read as what was
typed; the password reads as one mark per character, so that nothing of it
can be taken off the screen but its length.
-}
masked :: Field -> Text -> Text
masked = \case
  Password -> \typed -> T.replicate (T.length typed) "•"
  _ -> id
