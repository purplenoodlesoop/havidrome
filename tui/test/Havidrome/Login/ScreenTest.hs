{- | What the login screen shows for a form: the three fields under the
player's name, the field being typed into standing out, whatever the last
submit was told along the bottom, and the margin around all of it.

The forms here are built as they stand rather than typed into, because what
typing does to one is 'Havidrome.LoginTest''s; this is only what a form
reaches the terminal as.
-}
module Havidrome.Login.ScreenTest (tests) where

import Data.Text as T (Text)
import Data.Text qualified as T
import Havidrome.Check (Checks, example)
import Havidrome.Login (Field (Password, ServerUrl, Username), Form (..), blank)
import Havidrome.Login.Screen (draw, theme)
import Hedgehog (Gen, Group (Group), assert, forAll, property, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Terminal (Cell, border, highlighted, inside, screenshot, terminal, vacant)

tests :: Group
tests =
  Group
    "Havidrome.Login.Screen"
    ( drawing
        <> margin
        <> anyForm
    )

-- | What the screen draws for a form.
drawing :: Checks
drawing =
  [
    ( "draw fills the screen with the three fields under the player's name"
    , example do
        shown (40, 7) blank
          === ["havidrome", "", "Server URL", "Username", "Password", "", ""]
    )
  ,
    ( "draw shows what is typed next to the label of its field"
    , example do
        shown (40, 5) blank{username = "someone", focus = Username}
          === ["havidrome", "", "Server URL", "Username    someone", "Password"]
    )
  ,
    ( "draw never shows a character of the password"
    , example do
        let screen = shown (40, 5) blank{password = "secret", focus = Password}
        drop 4 screen === ["Password    ••••••"]
        assert (not (any (T.isInfixOf "secret") screen))
    )
  ,
    ( "draw marks the field being typed into, and no other"
    , example do
        marked (40, 7) blank{focus = ServerUrl} === ["Server URL"]
        marked (40, 7) blank{focus = Username} === ["Username"]
        marked (40, 7) blank{focus = Password} === ["Password"]
    )
  ,
    ( "draw keeps what the server said in the strip along the bottom"
    , example do
        drop 6 (shown (60, 7) refused)
          === ["The server refused these credentials: wrong password"]
    )
  ]

-- | The blank cell around the screen.
margin :: Checks
margin =
  [
    ( "the margin leaves the outer rows and columns blank, all of it inside"
    , example do
        assert (all vacant (border (whole (60, 9) refused)))
        screenshot (whole (60, 9) refused)
          === [ ""
              , " havidrome"
              , ""
              , " Server URL  https://music.example.org"
              , " Username    someone"
              , " Password    ••••••"
              , ""
              , " The server refused these credentials: wrong password"
              , ""
              ]
    )
  ,
    ( "the margin stays blank whatever the terminal's width and height"
    , property do
        region <- forAll size
        form <- forAll (Gen.element [blank, refused])
        filter (not . vacant) (border (whole region form)) === []
    )
  ]

-- | What holds of a form however it is filled in.
anyForm :: Checks
anyForm =
  [
    ( "the field being typed into is the only row that stands out, whatever is typed"
    , property do
        form <- forAll aForm
        let standing = marked (60, 7) form
        length standing === 1
        assert (all (T.isPrefixOf (label form.focus)) standing)
    )
  ,
    ( "no character of the password ever reaches the screen, whatever it is"
    , property do
        form <- forAll aForm
        let screen = shown (60, 7) form
            onScreen typed = any (T.isInfixOf (T.singleton typed)) screen
        assert (not (any onScreen (T.unpack form.password)))
    )
  ]

{- | Terminals from none at all, through ones too small to hold anything inside
their margin, to ones that hold the whole screen.
-}
size :: Gen (Int, Int)
size =
  (,)
    <$> Gen.choice [Gen.int (Range.linear 0 6), Gen.element [40, 60, 81]]
    <*> Gen.choice [Gen.int (Range.linear 0 6), Gen.element [9, 24]]

-- | A whole account filled in, with the server's refusal of it on screen.
refused :: Form
refused =
  blank
    { serverUrl = "https://music.example.org"
    , username = "someone"
    , password = "secret"
    , focus = Password
    , trouble = Just "The server refused these credentials: wrong password"
    }

{- | A form filled in however, with a password that is never empty, so that
there is always a character that must not reach the screen.

The password is written in digits and everything else in letters, so that a
digit anywhere on the screen can only have come from the password.
-}
aForm :: Gen Form
aForm = do
  serverUrl <- letters
  username <- letters
  password <- Gen.text (Range.linear 1 20) Gen.digit
  focus <- Gen.enumBounded
  trouble <- Gen.maybe (Gen.text (Range.linear 1 30) Gen.alpha)
  pure blank{serverUrl, username, password, focus, trouble}
 where
  letters = Gen.text (Range.linear 0 20) Gen.alpha

-- | The label the screen writes a field's name as.
label :: Field -> Text
label = \case
  ServerUrl -> "Server URL"
  Username -> "Username"
  Password -> "Password"

-- | Every cell of a terminal of this size.
whole :: (Int, Int) -> Form -> [[Cell]]
whole region = terminal theme region . draw

{- | Every cell inside the margin of a terminal with this many columns and rows
inside it.
-}
within :: (Int, Int) -> Form -> [[Cell]]
within (width, height) = inside . whole (width + 2, height + 2)

{- | What that terminal shows inside its margin, top row first, the blanks at
the ends trimmed.
-}
shown :: (Int, Int) -> Form -> [Text]
shown region = screenshot . within region

-- | The rows of the same screen that stand out.
marked :: (Int, Int) -> Form -> [Text]
marked region = highlighted . within region
