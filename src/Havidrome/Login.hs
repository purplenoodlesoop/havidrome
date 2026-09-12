-- | The login screen: the three details an account on a Navidrome server is
-- reached by, the keys that fill them in, and what submitting them does.
--
-- This is the screen a run with nothing stored opens on, and the screen a
-- logout comes back to. It ends in exactly one of two ways: a server took the
-- credentials, which are then stored and handed back for browsing, or the
-- player was left. Nothing here ever tries again on its own — a server is
-- asked once per submit, and only a key press asks it anything.
module Havidrome.Login
  ( -- * The form
    Form (..)
  , Field (..)
  , blank
  , value
  , ahead
  , back

    -- * The keys
  , Command (..)
  , command

    -- * Submitting
  , Entry (..)
  , navidrome
  , Ending (..)
  , step

    -- * The screen
  , Screen (..)
  , Name (..)
  , login
  , application

    -- * What it looks like
  , draw
  , theme
  , labelled
  , masked
  ) where

import Brick
  ( App (..)
  , AttrMap
  , AttrName
  , BrickEvent (VtyEvent)
  , EventM
  , Padding (Max)
  , Widget
  , attrMap
  , attrName
  , defaultMain
  , emptyWidget
  , fill
  , halt
  , neverShowCursor
  , padRight
  , txt
  , vBox
  , withAttr
  )
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (get, put)
import Data.Char (isPrint)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Havidrome.Credentials qualified as Credentials
import Havidrome.Margin (margined)
import Havidrome.Subsonic
  ( Credentials (Credentials)
  , Server (Server)
  , SubsonicError
  , checkCredentials
  , explain
  , newClient
  )

-- | One of the three details the screen asks for, in the order they are asked
-- in.
data Field
  = ServerUrl
  | Username
  | Password
  deriving stock (Eq, Ord, Show, Enum, Bounded)

-- | The screen as it has been filled in: what stands in each field, which
-- field the typing goes into, and whatever the last submit was told, which
-- sits in the strip along the bottom until the next key press.
data Form = Form
  { serverUrl :: Text
  , username :: Text
  , password :: Text
  , focus :: Field
  , trouble :: Maybe Text
  }
  deriving stock (Eq)

-- | Shows every field but the password, which is never among the characters
-- shown — not on the screen, and not in a line of output either.
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

-- | The form the screen opens on: three empty fields, the typing in the first
-- of them, nothing wrong yet.
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
  ServerUrl -> form {serverUrl = change form.serverUrl}
  Username -> form {username = change form.username}
  Password -> form {password = change form.password}

-- | The field after this one, round from the last back to the first.
ahead :: Field -> Field
ahead field = if field == maxBound then minBound else succ field

-- | The field before this one, round from the first back to the last.
back :: Field -> Field
back field = if field == minBound then maxBound else pred field

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

-- | The key map: Tab and the down arrow move to the next field, the up arrow
-- to the one before, Enter submits, Backspace takes a character back, and
-- Ctrl+C leaves the player. Every other printable character is typed into the
-- focused field — @q@ among them, which quits from nowhere in this player.
command :: Vty.Key -> [Vty.Modifier] -> Maybe Command
command key modifiers = case (key, modifiers) of
  (Vty.KChar 'c', [Vty.MCtrl]) -> Just Leave
  (Vty.KChar '\t', []) -> Just Ahead
  (Vty.KDown, []) -> Just Ahead
  (Vty.KUp, []) -> Just Back
  (Vty.KEnter, []) -> Just Submit
  (Vty.KBS, []) -> Just Rub
  (Vty.KChar character, []) | isPrint character -> Just (Type character)
  _ -> Nothing

-- | What submitting a filled-in form does with it: ask a server whether it
-- takes these credentials, and, when it does, keep them for later runs.
--
-- The player's is a Navidrome server and the config file ('navidrome').
-- Anything else that can answer and keep — a spec's stand-in, say — is as good
-- an entry as far as the screen is concerned, which is what the @f@ keeps
-- open.
data Entry f = Entry
  { accepts :: Credentials.Credentials -> f (Either SubsonicError ())
  -- ^ Whether the server these credentials name takes them.
  , keeps :: Credentials.Credentials -> f ()
  -- ^ Store them, so that later runs do not ask again.
  }

-- | The real entry: the server the typed URL names, asked over the network,
-- and the config file the accepted credentials are stored in.
navidrome :: Entry IO
navidrome =
  Entry
    { accepts = \credentials -> do
        client <-
          newClient
            (Server credentials.server)
            (Credentials credentials.username credentials.password)
        checkCredentials client
    , keeps = Credentials.save
    }

-- | How the login screen ended.
data Ending
  = -- | A server took these credentials, and they are stored.
    Entered Credentials.Credentials
  | -- | The player was left, with nothing stored.
    Abandoned
  deriving stock (Eq, Show)

-- | The form a command leaves behind, or the ending it brought about.
--
-- Submitting is the one command that asks a server anything, and it asks
-- exactly once. A refusal and a server that cannot be reached both leave the
-- form exactly as it was typed, ready to be typed over, with the reason in the
-- bottom strip and nothing stored; the screen then sits there until another
-- key is pressed.
step :: (Monad f) => Entry f -> Command -> Form -> f (Either Ending Form)
step entry instruction form = case instruction of
  Leave -> pure (Left Abandoned)
  Ahead -> stay quiet {focus = ahead form.focus}
  Back -> stay quiet {focus = back form.focus}
  Type character -> stay (alter form.focus (<> Text.singleton character) quiet)
  Rub -> stay (alter form.focus dropLast quiet)
  Submit ->
    entry.accepts filled >>= \case
      Right () -> entry.keeps filled >> pure (Left (Entered filled))
      Left failure -> stay quiet {trouble = Just (explain failure)}
  where
    -- Whatever a key press does, it first clears what the last one was told.
    quiet = form {trouble = Nothing}
    stay = pure . Right
    filled =
      Credentials.Credentials
        { Credentials.server = form.serverUrl
        , Credentials.username = form.username
        , Credentials.password = form.password
        }

dropLast :: Text -> Text
dropLast = Text.dropEnd 1

-- | The name brick knows the screen by. There is one thing on it, so there is
-- one name.
data Name = Prompt
  deriving stock (Eq, Ord, Show)

-- | Everything the screen is: the form being filled in, and the ending it
-- reached, once it has reached one.
data Screen = Screen
  { form :: Form
  , ending :: Maybe Ending
  }
  deriving stock (Eq, Show)

-- | Asks for credentials, and hands back the ones a server took — stored by
-- then — or nothing at all when the player was left.
login :: Entry IO -> IO (Maybe Credentials.Credentials)
login entry = do
  final <- defaultMain (application entry) (Screen blank Nothing)
  pure $ case final.ending of
    Just (Entered credentials) -> Just credentials
    Just Abandoned -> Nothing
    Nothing -> Nothing

-- | The screen itself, up until it is submitted successfully or left.
application :: Entry IO -> App Screen e Name
application entry =
  App
    { appDraw = draw . (.form)
    , appChooseCursor = neverShowCursor
    , appHandleEvent = handle entry
    , appStartEvent = pure ()
    , appAttrMap = const theme
    }

handle :: Entry IO -> BrickEvent Name e -> EventM Name Screen ()
handle entry = \case
  VtyEvent (Vty.EvKey key modifiers) ->
    case command key modifiers of
      Nothing -> pure ()
      Just instruction -> do
        screen <- get
        stepped <- liftIO (step entry instruction screen.form)
        case stepped of
          Left ended -> put screen {ending = Just ended} >> halt
          Right typed -> put screen {form = typed}
  _ -> pure ()

-- | The whole screen, inside the margin every screen has: the player's name,
-- the three fields under it with the focused one standing out, and the bottom
-- strip when a server has said something.
draw :: Form -> [Widget Name]
draw form =
  [ margined . vBox $
      [ withAttr titleAttribute (line "havidrome")
      , line " "
      ]
        <> map field [minBound .. maxBound]
        <> [ fill ' '
           , maybe emptyWidget (withAttr troubleAttribute . line) form.trouble
           ]
  ]
  where
    field which =
      standOut which (line (labelled which <> masked which (value which form)))
    standOut which = if form.focus == which then withAttr focusedAttribute else id

-- | A row of text across the full width, so that marking one covers the line
-- and not just its letters.
line :: Text -> Widget Name
line = padRight Max . txt

-- | What a field is called, in a column of its own so that what is typed into
-- the three of them lines up.
labelled :: Field -> Text
labelled =
  Text.justifyLeft 12 ' ' . \case
    ServerUrl -> "Server URL"
    Username -> "Username"
    Password -> "Password"

-- | What a field's contents read as on screen. Two of them read as what was
-- typed; the password reads as one mark per character, so that nothing of it
-- can be taken off the screen but its length.
masked :: Field -> Text -> Text
masked = \case
  Password -> \typed -> Text.replicate (Text.length typed) "•"
  _ -> id

-- | The focused field is the row in reverse video, as the selected row is when
-- browsing; the title is bold and the bottom strip red.
theme :: AttrMap
theme =
  attrMap
    Vty.defAttr
    [ (focusedAttribute, Vty.defAttr `Vty.withStyle` Vty.reverseVideo)
    , (titleAttribute, Vty.defAttr `Vty.withStyle` Vty.bold)
    , (troubleAttribute, Vty.defAttr `Vty.withForeColor` Vty.red)
    ]

titleAttribute, focusedAttribute, troubleAttribute :: AttrName
titleAttribute = attrName "title"
focusedAttribute = attrName "focused"
troubleAttribute = attrName "trouble"
