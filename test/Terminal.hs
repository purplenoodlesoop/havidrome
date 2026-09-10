{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedStrings #-}

-- | Rendering a widget the way brick renders it to a terminal, without a
-- terminal: what the screen would say, and which of its rows stand out.
module Terminal
  ( screenshot
  , highlighted
  , inReverse
  , inBold
  ) where

import Brick (AttrMap, Widget)
import Brick.Main (renderWidget)
import Data.Function (on)
import Data.List (groupBy)
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
  map (Text.stripEnd . text) . filter (any (drawnIn reverseVideo)) . rows theme region

-- | Each stretch of the same screen drawn in reverse video, top row first and
-- left to right along a row: where a row has several things side by side,
-- only the one in reverse video.
inReverse :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [Text]
inReverse = stretches reverseVideo

-- | Each stretch of it drawn in bold, the same way.
inBold :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [Text]
inBold = stretches bold

stretches :: (Ord name) => Style -> AttrMap -> DisplayRegion -> [Widget name] -> [Text]
stretches style theme region =
  filter (not . Text.null)
    . map (Text.stripEnd . text)
    . concatMap (filter (all (drawnIn style)) . groupBy ((==) `on` drawnIn style))
    . rows theme region

drawnIn :: Style -> SpanOp -> Bool
drawnIn wanted = \case
  TextSpan {textSpanAttr = attribute} -> case attrStyle attribute of
    SetTo style -> hasStyle style wanted
    _ -> False
  _ -> False

rows :: (Ord name) => AttrMap -> DisplayRegion -> [Widget name] -> [[SpanOp]]
rows theme region widgets =
  map Vector.toList (Vector.toList (displayOpsForPic (renderWidget (Just theme) widgets region) region))

text :: [SpanOp] -> Text
text = foldMap $ \case
  TextSpan {textSpanText = written} -> Lazy.toStrict written
  Skip columns -> Text.replicate columns " "
  RowEnd columns -> Text.replicate columns " "
