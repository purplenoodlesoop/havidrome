-- | The login screen: what the keys do, what a submit does with what was
-- typed, and what the terminal shows.
--
-- The server and the config file here are a stand-in that writes down what it
-- was asked and what it was told to keep, so a spec sees exactly what a submit
-- did — and, after a refusal, that it did nothing.
module Havidrome.LoginSpec (spec) where

import Control.Monad (foldM, forM_)
import Control.Monad.Trans.State.Strict (State, modify', runState)
import Data.Text (Text)
import Data.Text qualified as Text
import Graphics.Vty qualified as Vty
import Havidrome.Credentials (Credentials (Credentials))
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
  , draw
  , labelled
  , masked
  , step
  , theme
  , value
  )
import Havidrome.Subsonic (SubsonicError (AuthRejected, NetworkFailure))
import Terminal (Cell, border, highlighted, inside, screenshot, terminal, vacant)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "command" $ do
    it "moves to the next field on Tab and on the down arrow" $ do
      command (Vty.KChar '\t') [] `shouldBe` Just Ahead
      command Vty.KDown [] `shouldBe` Just Ahead

    it "moves to the field before on the up arrow" $
      command Vty.KUp [] `shouldBe` Just Back

    it "submits on Enter" $
      command Vty.KEnter [] `shouldBe` Just Submit

    it "leaves the player on Ctrl+C" $
      command (Vty.KChar 'c') [Vty.MCtrl] `shouldBe` Just Leave

    it "types a printable character into the field" $ do
      command (Vty.KChar 'x') [] `shouldBe` Just (Type 'x')
      command (Vty.KChar ' ') [] `shouldBe` Just (Type ' ')
      command (Vty.KChar '!') [] `shouldBe` Just (Type '!')

    it "types q like any other character, quitting from nowhere" $
      command (Vty.KChar 'q') [] `shouldBe` Just (Type 'q')

    it "takes a character back on Backspace" $
      command Vty.KBS [] `shouldBe` Just Rub

    it "ignores every other key" $ do
      command Vty.KEsc [] `shouldBe` Nothing
      command Vty.KLeft [] `shouldBe` Nothing
      command (Vty.KChar 'q') [Vty.MCtrl] `shouldBe` Nothing

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

  describe "draw" $ do
    it "fills the screen with the three fields under the player's name" $
      shown (40, 7) (typing [])
        `shouldBe` ["havidrome", "", "Server URL", "Username", "Password", "", ""]

    it "shows what is typed next to the label of its field" $
      shown (40, 5) (typing (typedInto Username "someone"))
        `shouldBe` ["havidrome", "", "Server URL", "Username    someone", "Password"]

    it "never shows a character of the password" $ do
      let screen = shown (40, 5) (typing (typedInto Password "secret"))
      screen !! 4 `shouldBe` "Password    ••••••"
      screen `shouldSatisfy` all (not . Text.isInfixOf "secret")

    it "masks the password and nothing else" $ do
      masked Password "secret" `shouldBe` "••••••"
      masked ServerUrl "secret" `shouldBe` "secret"
      masked Username "secret" `shouldBe` "secret"

    it "marks the field being typed into, and no other" $ do
      marked (40, 7) (typing []) `shouldBe` ["Server URL"]
      marked (40, 7) (typing [Ahead]) `shouldBe` ["Username"]
      marked (40, 7) (typing [Back]) `shouldBe` ["Password"]

    it "keeps what the server said in the strip along the bottom" $
      last (shown (60, 7) (fst (answering refusal details)))
        `shouldBe` "The server refused these credentials: wrong password"

    it "lines the three labels up in a column of their own" $
      map (Text.length . labelled) fields `shouldBe` [12, 12, 12]

  describe "the margin" $ do
    it "leaves the terminal's outer rows and columns blank, the title, fields and error inside" $ do
      let refused = fst (answering refusal details)
      border (whole (60, 9) refused) `shouldSatisfy` all vacant
      screenshot (whole (60, 9) refused)
        `shouldBe` [ ""
                   , " havidrome"
                   , ""
                   , " Server URL  https://music.example.org"
                   , " Username    someone"
                   , " Password    ••••••"
                   , ""
                   , " The server refused these credentials: wrong password"
                   , ""
                   ]

    it "stays blank whatever the terminal's width and height" $
      forM_ [typing [], fst (answering refusal details)] $ \screen ->
        forM_ sizes $ \region ->
          (region, filter (not . vacant) (border (whole region screen))) `shouldBe` (region, [])

-- | Terminals from none at all, through ones too small to hold anything inside
-- their margin, to ones that hold the whole screen.
sizes :: [(Int, Int)]
sizes = [(width, height) | width <- [0 .. 6] <> [40, 81], height <- [0 .. 6] <> [9, 24]]

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

-- | Every cell of a terminal of this size.
whole :: (Int, Int) -> Either Ending Form -> [[Cell]]
whole region = either (const []) (terminal theme region . draw)

-- | Every cell inside the margin of a terminal with this many columns and rows
-- inside it.
within :: (Int, Int) -> Either Ending Form -> [[Cell]]
within (width, height) = inside . whole (width + 2, height + 2)

-- | What that terminal shows inside its margin, top row first, the blanks at
-- the ends trimmed.
shown :: (Int, Int) -> Either Ending Form -> [Text]
shown region = screenshot . within region

-- | The rows of the same screen that stand out.
marked :: (Int, Int) -> Either Ending Form -> [Text]
marked region = highlighted . within region
