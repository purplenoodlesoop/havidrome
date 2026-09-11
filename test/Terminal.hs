{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedStrings #-}

-- | Rendering a widget the way brick renders it to a terminal, without a
-- terminal: what the screen would say, and which of its rows stand out.
module Terminal
  ( screenshot
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
  , hasStyle
  , reverseVideo
  )
import Graphics.Vty.PictureToSpans (displayOpsForPic)
import Graphics.Vty.Span (SpanOp (RowEnd, Skip, TextSpan), textSpanAttr, textSpanText)

-- | Every row of a screen of this size, with the blanks at the end of each row
-- dropped so that a row reads as what was written on it.
screenshot :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [Text]
screenshot theme region = map (Text.stripEnd . text) . rows theme region

-- | The rows of the same screen that are drawn in reverse video — where the
-- selection is.
highlighted :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [Text]
highlighted theme region =
  map (Text.stripEnd . text) . filter (any (reversed . attributeOf)) . rows theme region

-- | Each stretch of the same screen drawn in bold, top row first and left to
-- right along a row: where a row has several things side by side, only the one
-- in bold.
inBold :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [Text]
inBold theme region =
  filter (not . Text.null)
    . map (Text.stripEnd . text)
    . concatMap (filter (all emboldened) . groupBy ((==) `on` emboldened))
    . rows theme region
  where
    emboldened = drawnIn bold . attributeOf

-- | Every row of the same screen as the runs along it that are drawn alike,
-- left to right: how each run is drawn — nothing where nothing was — and what
-- it says, blanks included.
runs :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [[(Maybe Attr, Text)]]
runs theme region =
  map (map run . NonEmpty.groupBy ((==) `on` attributeOf)) . rows theme region
  where
    run ops = (attributeOf (NonEmpty.head ops), text (NonEmpty.toList ops))

-- | Whether what is drawn so is in reverse video.
reversed :: Maybe Attr -> Bool
reversed = drawnIn reverseVideo

drawnIn :: Style -> Maybe Attr -> Bool
drawnIn wanted = \case
  Just attribute | SetTo style <- attrStyle attribute -> hasStyle style wanted
  _ -> False

attributeOf :: SpanOp -> Maybe Attr
attributeOf = \case
  TextSpan {textSpanAttr = attribute} -> Just attribute
  _ -> Nothing

rows :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [[SpanOp]]
rows theme region widgets =
  map Vector.toList (Vector.toList (displayOpsForPic (renderWidget (Just theme) widgets region) region))

text :: [SpanOp] -> Text
text = foldMap $ \case
  TextSpan {textSpanText = written} -> Lazy.toStrict written
  Skip columns -> Text.replicate columns " "
  RowEnd columns -> Text.replicate columns " "
