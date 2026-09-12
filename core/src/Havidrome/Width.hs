-- | How many columns a character takes on a terminal, how many a line of
-- text takes, and what is left of a line cut to the columns it has. It is the
-- one measure the whole player lays out against: the now-playing overlay
-- shares the width it is given out between the parts of its line, and a row
-- too long for its column is cut to the width that is left.
--
-- The rule is Markus Kuhn's @wcwidth@ (2007-05-26, Unicode 5.0), which is the
-- same rule the terminal library draws by, written out here so that the core
-- measures a line without reaching for a terminal. Its permission notice
-- grants use, copying, modification and distribution for any purpose.
--
-- A character no terminal advances the cursor for — a control character, a
-- combining mark, a zero-width space — takes no column at all; an East Asian
-- wide or full-width character takes two; everything else takes one.
module Havidrome.Width
  ( char
  , text
  , shorten
  ) where

import Data.Char (isControl)
import Data.Text (Text)
import Data.Text qualified as Text

-- | The columns one character takes.
char :: Char -> Int
char character
  | code < 0x0020 = 0 -- The null character and the C0 controls.
  | code < 0x007F = 1 -- The rest of ASCII, which is most of what is measured.
  | code < 0x00A0 = 0 -- Delete and the C1 controls.
  | combining code = 0
  | wide code = 2
  | otherwise = 1
  where
    code = fromEnum character

-- | The columns a line of text takes, which is what its characters take
-- between them.
text :: Text -> Int
text = Text.foldl' (\taken character -> taken + char character) 0

-- | Text on one line of at most this many columns. What does not fit is cut
-- off, and an ellipsis at the end says so. A character that would move the
-- terminal onto another line, or anywhere else, is a space instead.
shorten :: Int -> Text -> Text
shorten room said
  | text flat <= room = flat
  | room < text ellipsis = Text.empty
  | otherwise = fitting (room - text ellipsis) flat <> ellipsis
  where
    flat = Text.map (\character -> if isControl character then ' ' else character) said
    ellipsis = "…"

-- | The longest start of the text that takes at most this many columns.
fitting :: Int -> Text -> Text
fitting room said = Text.take (length (takeWhile (<= room) reaches)) said
  where
    reaches = scanl1 (+) (map char (Text.unpack said))

-- | Whether a character hangs off the one before it rather than taking a
-- column of its own: a non-spacing or enclosing mark, a format character, a
-- Hangul Jamo medial vowel or final consonant, or a zero-width space.
combining :: Int -> Bool
combining code = within marks
  where
    within = \case
      [] -> False
      (lowest, highest) : rest
        | code < lowest -> False
        | code <= highest -> True
        | otherwise -> within rest

-- | Whether a character takes two columns: the East Asian wide and full-width
-- classes of Unicode Technical Report #11.
wide :: Int -> Bool
wide code =
  code >= 0x1100
    && ( code <= 0x115F -- Hangul Jamo initial consonants.
          || code == 0x2329
          || code == 0x232A
          || (code >= 0x2E80 && code <= 0xA4CF && code /= 0x303F) -- CJK to Yi.
          || (code >= 0xAC00 && code <= 0xD7A3) -- Hangul syllables.
          || (code >= 0xF900 && code <= 0xFAFF) -- CJK compatibility ideographs.
          || (code >= 0xFE10 && code <= 0xFE19) -- Vertical forms.
          || (code >= 0xFE30 && code <= 0xFE6F) -- CJK compatibility forms.
          || (code >= 0xFF00 && code <= 0xFF60) -- Full-width forms.
          || (code >= 0xFFE0 && code <= 0xFFE6)
          || (code >= 0x20000 && code <= 0x2FFFD)
          || (code >= 0x30000 && code <= 0x3FFFD)
       )

-- | The characters that take no column, as the ranges they fall in, lowest
-- first and none overlapping another. Kuhn generated them from the Unicode
-- database as the general categories @Me@ and @Mn@, and @Cf@ but for the soft
-- hyphen, together with @U+1160-U+11FF@ and @U+200B@.
marks :: [(Int, Int)]
marks =
  [ (0x0300, 0x036F)
  , (0x0483, 0x0486)
  , (0x0488, 0x0489)
  , (0x0591, 0x05BD)
  , (0x05BF, 0x05BF)
  , (0x05C1, 0x05C2)
  , (0x05C4, 0x05C5)
  , (0x05C7, 0x05C7)
  , (0x0600, 0x0603)
  , (0x0610, 0x0615)
  , (0x064B, 0x065E)
  , (0x0670, 0x0670)
  , (0x06D6, 0x06E4)
  , (0x06E7, 0x06E8)
  , (0x06EA, 0x06ED)
  , (0x070F, 0x070F)
  , (0x0711, 0x0711)
  , (0x0730, 0x074A)
  , (0x07A6, 0x07B0)
  , (0x07EB, 0x07F3)
  , (0x0901, 0x0902)
  , (0x093C, 0x093C)
  , (0x0941, 0x0948)
  , (0x094D, 0x094D)
  , (0x0951, 0x0954)
  , (0x0962, 0x0963)
  , (0x0981, 0x0981)
  , (0x09BC, 0x09BC)
  , (0x09C1, 0x09C4)
  , (0x09CD, 0x09CD)
  , (0x09E2, 0x09E3)
  , (0x0A01, 0x0A02)
  , (0x0A3C, 0x0A3C)
  , (0x0A41, 0x0A42)
  , (0x0A47, 0x0A48)
  , (0x0A4B, 0x0A4D)
  , (0x0A70, 0x0A71)
  , (0x0A81, 0x0A82)
  , (0x0ABC, 0x0ABC)
  , (0x0AC1, 0x0AC5)
  , (0x0AC7, 0x0AC8)
  , (0x0ACD, 0x0ACD)
  , (0x0AE2, 0x0AE3)
  , (0x0B01, 0x0B01)
  , (0x0B3C, 0x0B3C)
  , (0x0B3F, 0x0B3F)
  , (0x0B41, 0x0B43)
  , (0x0B4D, 0x0B4D)
  , (0x0B56, 0x0B56)
  , (0x0B82, 0x0B82)
  , (0x0BC0, 0x0BC0)
  , (0x0BCD, 0x0BCD)
  , (0x0C3E, 0x0C40)
  , (0x0C46, 0x0C48)
  , (0x0C4A, 0x0C4D)
  , (0x0C55, 0x0C56)
  , (0x0CBC, 0x0CBC)
  , (0x0CBF, 0x0CBF)
  , (0x0CC6, 0x0CC6)
  , (0x0CCC, 0x0CCD)
  , (0x0CE2, 0x0CE3)
  , (0x0D41, 0x0D43)
  , (0x0D4D, 0x0D4D)
  , (0x0DCA, 0x0DCA)
  , (0x0DD2, 0x0DD4)
  , (0x0DD6, 0x0DD6)
  , (0x0E31, 0x0E31)
  , (0x0E34, 0x0E3A)
  , (0x0E47, 0x0E4E)
  , (0x0EB1, 0x0EB1)
  , (0x0EB4, 0x0EB9)
  , (0x0EBB, 0x0EBC)
  , (0x0EC8, 0x0ECD)
  , (0x0F18, 0x0F19)
  , (0x0F35, 0x0F35)
  , (0x0F37, 0x0F37)
  , (0x0F39, 0x0F39)
  , (0x0F71, 0x0F7E)
  , (0x0F80, 0x0F84)
  , (0x0F86, 0x0F87)
  , (0x0F90, 0x0F97)
  , (0x0F99, 0x0FBC)
  , (0x0FC6, 0x0FC6)
  , (0x102D, 0x1030)
  , (0x1032, 0x1032)
  , (0x1036, 0x1037)
  , (0x1039, 0x1039)
  , (0x1058, 0x1059)
  , (0x1160, 0x11FF)
  , (0x135F, 0x135F)
  , (0x1712, 0x1714)
  , (0x1732, 0x1734)
  , (0x1752, 0x1753)
  , (0x1772, 0x1773)
  , (0x17B4, 0x17B5)
  , (0x17B7, 0x17BD)
  , (0x17C6, 0x17C6)
  , (0x17C9, 0x17D3)
  , (0x17DD, 0x17DD)
  , (0x180B, 0x180D)
  , (0x18A9, 0x18A9)
  , (0x1920, 0x1922)
  , (0x1927, 0x1928)
  , (0x1932, 0x1932)
  , (0x1939, 0x193B)
  , (0x1A17, 0x1A18)
  , (0x1B00, 0x1B03)
  , (0x1B34, 0x1B34)
  , (0x1B36, 0x1B3A)
  , (0x1B3C, 0x1B3C)
  , (0x1B42, 0x1B42)
  , (0x1B6B, 0x1B73)
  , (0x1DC0, 0x1DCA)
  , (0x1DFE, 0x1DFF)
  , (0x200B, 0x200F)
  , (0x202A, 0x202E)
  , (0x2060, 0x2063)
  , (0x206A, 0x206F)
  , (0x20D0, 0x20EF)
  , (0x302A, 0x302F)
  , (0x3099, 0x309A)
  , (0xA806, 0xA806)
  , (0xA80B, 0xA80B)
  , (0xA825, 0xA826)
  , (0xFB1E, 0xFB1E)
  , (0xFE00, 0xFE0F)
  , (0xFE20, 0xFE23)
  , (0xFEFF, 0xFEFF)
  , (0xFFF9, 0xFFFB)
  , (0x10A01, 0x10A03)
  , (0x10A05, 0x10A06)
  , (0x10A0C, 0x10A0F)
  , (0x10A38, 0x10A3A)
  , (0x10A3F, 0x10A3F)
  , (0x1D167, 0x1D169)
  , (0x1D173, 0x1D182)
  , (0x1D185, 0x1D18B)
  , (0x1D1AA, 0x1D1AD)
  , (0x1D242, 0x1D244)
  , (0xE0001, 0xE0001)
  , (0xE0020, 0xE007F)
  , (0xE0100, 0xE01EF)
  ]
