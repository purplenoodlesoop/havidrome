{-# LANGUAGE LambdaCase #-}

{-# LANGUAGE OverloadedStrings #-}

-- | Rendering a widget the way brick renders it to a terminal, without a
-- terminal: what the screen would say, and which of its rows stand out.
module Terminal
  ( screenshot
  , highlighted
  ) where

import Brick (AttrMap, Widget)
import Brick.Main (renderWidget)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as Lazy
import Data.Vector qualified as Vector
import Graphics.Vty (DisplayRegion)
import Graphics.Vty.Attributes (Attr (attrStyle), MaybeDefault (SetTo), hasStyle, reverseVideo)
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
  map (Text.stripEnd . text) . filter (any reversed) . rows theme region
  where
    reversed = \case
      TextSpan {textSpanAttr = attribute} -> case attrStyle attribute of
        SetTo style -> hasStyle style reverseVideo
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
