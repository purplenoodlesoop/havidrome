-- | What the login screen shows for a form: the three fields under the
-- player's name, the field being typed into standing out, whatever the last
-- submit was told along the bottom, and the margin around all of it.
--
-- The forms here are built as they stand rather than typed into, because what
-- typing does to one is 'Havidrome.LoginSpec''s; this is only what a form
-- reaches the terminal as.
module Havidrome.Login.ScreenSpec (spec) where

import Control.Monad (forM_)
import Data.Text (Text)
import Data.Text qualified as Text
import Havidrome.Login (Field (Password, ServerUrl, Username), Form (..), blank)
import Havidrome.Login.Screen (draw, theme)
import Terminal (Cell, border, highlighted, inside, screenshot, terminal, vacant)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

spec :: Spec
spec = do
  describe "draw" $ do
    it "fills the screen with the three fields under the player's name" $
      shown (40, 7) blank
        `shouldBe` ["havidrome", "", "Server URL", "Username", "Password", "", ""]

    it "shows what is typed next to the label of its field" $
      shown (40, 5) blank {username = "someone", focus = Username}
        `shouldBe` ["havidrome", "", "Server URL", "Username    someone", "Password"]

    it "never shows a character of the password" $ do
      let screen = shown (40, 5) blank {password = "secret", focus = Password}
      screen !! 4 `shouldBe` "Password    ••••••"
      screen `shouldSatisfy` all (not . Text.isInfixOf "secret")

    it "marks the field being typed into, and no other" $ do
      marked (40, 7) blank {focus = ServerUrl} `shouldBe` ["Server URL"]
      marked (40, 7) blank {focus = Username} `shouldBe` ["Username"]
      marked (40, 7) blank {focus = Password} `shouldBe` ["Password"]

    it "keeps what the server said in the strip along the bottom" $
      last (shown (60, 7) refused)
        `shouldBe` "The server refused these credentials: wrong password"

  describe "the margin" $ do
    it "leaves the terminal's outer rows and columns blank, the title, fields and error inside" $ do
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
      forM_ [blank, refused] $ \screen ->
        forM_ sizes $ \region ->
          (region, filter (not . vacant) (border (whole region screen))) `shouldBe` (region, [])

-- | Terminals from none at all, through ones too small to hold anything inside
-- their margin, to ones that hold the whole screen.
sizes :: [(Int, Int)]
sizes = [(width, height) | width <- [0 .. 6] <> [40, 81], height <- [0 .. 6] <> [9, 24]]

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

-- | Every cell of a terminal of this size.
whole :: (Int, Int) -> Form -> [[Cell]]
whole region = terminal theme region . draw

-- | Every cell inside the margin of a terminal with this many columns and rows
-- inside it.
within :: (Int, Int) -> Form -> [[Cell]]
within (width, height) = inside . whole (width + 2, height + 2)

-- | What that terminal shows inside its margin, top row first, the blanks at
-- the ends trimmed.
shown :: (Int, Int) -> Form -> [Text]
shown region = screenshot . within region

-- | The rows of the same screen that stand out.
marked :: (Int, Int) -> Form -> [Text]
marked region = highlighted . within region
