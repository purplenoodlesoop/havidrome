{- | The list functions the suite reaches for that @base@ does not have, under
the names @Data.List.Extra@ gives them. They are here rather than taken from
that package because these four are all of it the suite wants.
-}
module Lists
  ( drop1
  , dropEnd1
  , takeEnd
  ) where

{- | Everything of a list but the first, and everything but the last. An empty
list has neither, and is left empty.
-}
drop1, dropEnd1 :: [a] -> [a]
drop1 = drop 1
dropEnd1 = dropEnd 1

{- | Everything but the last so many, counted off the front as they are
reached, so that a list shorter than that comes back empty.
-}
dropEnd :: Int -> [a] -> [a]
dropEnd count items = zipWith const items (drop count items)

-- | The last so many of a list, or all of it if it is shorter than that.
takeEnd :: Int -> [a] -> [a]
takeEnd count items = drop (length items - count) items
