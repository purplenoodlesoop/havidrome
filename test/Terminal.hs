{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering a widget the way brick renders it to a terminal, without a
-- terminal: what the screen would say, which of its rows stand out, and what
-- is at its edges.
--
-- A screen is taken in as its cells, one to a character, which keeps them in
-- line with the terminal's columns for as long as no character on it is a
-- wide one.
module Terminal
  ( -- * The cells of a screen
    Cell
  , terminal
  , inside
  , border
  , vacant

    -- * What they show
  , screenshot
  , highlighted
  , inBold
  , runs
  , reversed
  ) where

import Brick (AttrMap, Widget)
import Brick.Main (renderWidget)
import Data.Function (on)
import Data.List (groupBy)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Vector qualified as Vector
import Graphics.Vty (DisplayRegion)
import Graphics.Vty.Attributes
  ( Attr (attrStyle)
  , MaybeDefault (SetTo)
  , Style
  , bold
  , defAttr
  , hasStyle
  , reverseVideo
  )
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp (RowEnd, Skip, TextSpan), textSpanAttr, textSpanText)

-- | One cell of a screen: how it is drawn — nothing where nothing was — and
-- the character in it.
type Cell = (Maybe Attr, Char)

-- | Every row of a screen of this size, top row first, as the cells along it,
-- left to right.
terminal :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [[Cell]]
terminal theme region widgets =
  map (concatMap cells . Vector.toList) (Vector.toList (displayOpsForPic picture region))
  where
    picture = renderWidget (Just theme) widgets region
    cells = \case
      TextSpan {textSpanAttr, textSpanText} -> map (Just textSpanAttr,) (Lazy.unpack textSpanText)
      Skip columns -> replicate columns nothing
      RowEnd columns -> replicate columns nothing
    nothing = (Nothing, ' ')

-- | The same rows with the outermost row and column taken off every side:
-- what is within a margin of one cell.
inside :: [[Cell]] -> [[Cell]]
inside = map (dropEnd 1 . drop 1) . dropEnd 1 . drop 1

-- | The cells along the edges of the same rows: the top and bottom rows, and
-- the leftmost and rightmost cell of every row.
border :: [[Cell]] -> [Cell]
border rows = concat (ends rows) <> concatMap ends rows
  where
    ends line = take 1 line <> takeEnd 1 line

-- | Whether a cell shows nothing: a space, drawn as the terminal draws what it
-- is given no look for, or not drawn at all.
vacant :: Cell -> Bool
vacant (look, character) = character == ' ' && maybe True (== defAttr) look

-- | Every row, with the blanks at the end of each row dropped so that a row
-- reads as what was written on it.
screenshot :: [[Cell]] -> [Text]
screenshot = map (Text.stripEnd . text)

-- | The rows drawn in reverse video — where the selection is.
highlighted :: [[Cell]] -> [Text]
highlighted = screenshot . filter (any (reversed . fst))

-- | Each stretch drawn in bold, top row first and left to right along a row:
-- where a row has several things side by side, only the one in bold.
inBold :: [[Cell]] -> [Text]
inBold =
  filter (not . Text.null)
    . map (Text.stripEnd . text)
    . concatMap (filter (all emboldened) . groupBy ((==) `on` emboldened))
  where
    emboldened = drawnIn bold . fst

-- | Every row as the runs along it that are drawn alike, left to right: how
-- each run is drawn and what it says, blanks included.
runs :: [[Cell]] -> [[(Maybe Attr, Text)]]
runs = map (map run . NonEmpty.groupBy ((==) `on` fst))
  where
    run alike = (fst (NonEmpty.head alike), text (NonEmpty.toList alike))

-- | Whether what is drawn so is in reverse video.
reversed :: Maybe Attr -> Bool
reversed = drawnIn reverseVideo

drawnIn :: Style -> Maybe Attr -> Bool
drawnIn wanted = \case
  Just attribute | SetTo style <- attrStyle attribute -> hasStyle style wanted
  _ -> False

text :: [Cell] -> Text
text = Text.pack . map snd

dropEnd :: Int -> [a] -> [a]
dropEnd count items = zipWith const items (drop count items)

takeEnd :: Int -> [a] -> [a]
takeEnd count items = drop (length items - count) items
