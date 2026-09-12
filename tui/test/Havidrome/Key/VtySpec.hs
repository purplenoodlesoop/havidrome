-- | What the terminal reports, as the screens read it: every key the player
-- binds arrives as itself, every modifier it looks at survives the way in,
-- and anything else arrives as nothing at all.
module Havidrome.Key.VtySpec (spec) where

import Graphics.Vty qualified as Vty
import Havidrome.Key (Key (..), Modifier (..))
import Havidrome.Key.Vty (pressed)
import Test.Hspec (Spec, describe, it, shouldBe)

spec :: Spec
spec = describe "pressed" $ do
  it "reads the keys the login screen binds" $ do
    pressed (Vty.KChar '\t') [] `shouldBe` Just (Character '\t', [])
    pressed Vty.KDown [] `shouldBe` Just (DownArrow, [])
    pressed Vty.KUp [] `shouldBe` Just (UpArrow, [])
    pressed Vty.KEnter [] `shouldBe` Just (Enter, [])
    pressed Vty.KBS [] `shouldBe` Just (Backspace, [])
    pressed (Vty.KChar 'x') [] `shouldBe` Just (Character 'x', [])

  it "reads the keys the browsing screen binds" $ do
    pressed Vty.KEsc [] `shouldBe` Just (Escape, [])
    pressed Vty.KLeft [] `shouldBe` Just (LeftArrow, [])
    pressed Vty.KRight [] `shouldBe` Just (RightArrow, [])
    pressed (Vty.KChar ' ') [] `shouldBe` Just (Character ' ', [])

  it "reads the modifiers the screens look at" $ do
    pressed (Vty.KChar 'c') [Vty.MCtrl] `shouldBe` Just (Character 'c', [Ctrl])
    pressed Vty.KRight [Vty.MShift] `shouldBe` Just (RightArrow, [Shift])
    pressed (Vty.KChar 'n') [Vty.MMeta] `shouldBe` Just (Character 'n', [Meta])
    pressed (Vty.KChar 'n') [Vty.MAlt] `shouldBe` Just (Character 'n', [Alt])

  it "reads a key no screen binds as nothing at all" $ do
    pressed (Vty.KFun 1) [] `shouldBe` Nothing
    pressed Vty.KHome [] `shouldBe` Nothing
    pressed Vty.KPageUp [] `shouldBe` Nothing
