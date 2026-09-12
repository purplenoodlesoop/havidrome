{- | What the terminal reports, as the screens read it: every key the player
binds arrives as itself, every modifier it looks at survives the way in,
and anything else arrives as nothing at all.
-}
module Havidrome.Key.VtyTest (tests) where

import Graphics.Vty qualified as Vty
import Havidrome.Check (Checks, example)
import Havidrome.Key (Key (Backspace, Character, DownArrow, Enter, Escape, LeftArrow, RightArrow, UpArrow), Modifier (Alt, Ctrl, Meta, Shift))
import Havidrome.Key.Vty (pressed)
import Hedgehog (Gen, Group (Group), assert, forAll, property, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

tests :: Group
tests =
  Group
    "Havidrome.Key.Vty"
    ( reading
        <> anyPress
    )

-- | The key presses the screens bind, and the ones they do not.
reading :: Checks
reading =
  [
    ( "reads the keys the login screen binds"
    , example do
        pressed (Vty.KChar '\t') [] === Just (Character '\t', [])
        pressed Vty.KDown [] === Just (DownArrow, [])
        pressed Vty.KUp [] === Just (UpArrow, [])
        pressed Vty.KEnter [] === Just (Enter, [])
        pressed Vty.KBS [] === Just (Backspace, [])
        pressed (Vty.KChar 'x') [] === Just (Character 'x', [])
    )
  ,
    ( "reads the keys the browsing screen binds"
    , example do
        pressed Vty.KEsc [] === Just (Escape, [])
        pressed Vty.KLeft [] === Just (LeftArrow, [])
        pressed Vty.KRight [] === Just (RightArrow, [])
        pressed (Vty.KChar ' ') [] === Just (Character ' ', [])
    )
  ,
    ( "reads the modifiers the screens look at"
    , example do
        pressed (Vty.KChar 'c') [Vty.MCtrl] === Just (Character 'c', [Ctrl])
        pressed Vty.KRight [Vty.MShift] === Just (RightArrow, [Shift])
        pressed (Vty.KChar 'n') [Vty.MMeta] === Just (Character 'n', [Meta])
        pressed (Vty.KChar 'n') [Vty.MAlt] === Just (Character 'n', [Alt])
    )
  ,
    ( "reads a key no screen binds as nothing at all"
    , example do
        pressed (Vty.KFun 1) [] === Nothing
        pressed Vty.KHome [] === Nothing
        pressed Vty.KPageUp [] === Nothing
    )
  ,
    ( "reads whatever character is typed as that character"
    , property do
        typed <- forAll Gen.unicode
        modifiers <- forAll heldDown
        fmap fst (pressed (Vty.KChar typed) modifiers) === Just (Character typed)
    )
  ]

-- | What holds of any key press vty reports.
anyPress :: Checks
anyPress =
  [
    ( "reads every key outside the ones the screens bind as nothing at all"
    , property do
        key <- forAll unbound
        modifiers <- forAll heldDown
        pressed key modifiers === Nothing
    )
  ,
    ( "keeps every modifier held with a bound key, in the order they were held"
    , property do
        key <- forAll bound
        modifiers <- forAll heldDown
        fmap snd (pressed key modifiers)
          === Just (concatMap (\one -> foldMap snd (pressed key [one])) modifiers)
    )
  ,
    ( "tells vty's modifiers apart: two different ones never arrive as one"
    , property do
        key <- forAll bound
        one <- forAll modifier
        another <- forAll (Gen.filter (/= one) modifier)
        pressed key [one] /== pressed key [another]
    )
  ,
    ( "reads a modifier held on its own as exactly one modifier"
    , property do
        key <- forAll bound
        one <- forAll modifier
        assert (fmap (length . snd) (pressed key [one]) == Just 1)
    )
  ]

-- | One of the keys the player binds, as vty reports it.
bound :: Gen Vty.Key
bound =
  Gen.choice
    [ Vty.KChar <$> Gen.unicode
    , Gen.element
        [Vty.KEnter, Vty.KEsc, Vty.KBS, Vty.KUp, Vty.KDown, Vty.KLeft, Vty.KRight]
    ]

-- | A key vty can report that no screen binds.
unbound :: Gen Vty.Key
unbound =
  Gen.choice
    [ Vty.KFun <$> Gen.int (Range.linear 1 12)
    , Gen.element
        [ Vty.KHome
        , Vty.KEnd
        , Vty.KPageUp
        , Vty.KPageDown
        , Vty.KDel
        , Vty.KIns
        , Vty.KBackTab
        , Vty.KCenter
        , Vty.KPrtScr
        , Vty.KPause
        , Vty.KBegin
        , Vty.KMenu
        ]
    ]

-- | One of the modifiers the screens look at, as vty reports it.
modifier :: Gen Vty.Modifier
modifier = Gen.element [Vty.MShift, Vty.MCtrl, Vty.MMeta, Vty.MAlt]

-- | The modifiers held down with a key: any number of them, in any order.
heldDown :: Gen [Vty.Modifier]
heldDown = Gen.list (Range.linear 0 4) modifier
