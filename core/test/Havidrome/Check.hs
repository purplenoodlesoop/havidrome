-- | What a group of this suite is built out of, beside hedgehog's own
-- 'Hedgehog.property': a case that is genuinely one case.
module Havidrome.Check
  ( example
  ) where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (Property, PropertyT, property, withTests)

-- | One case, checked once: an example the spec names, or a piece of output
-- pinned exactly as it reads. There is nothing generated in it, so generating
-- it a hundred times would check the same case a hundred times.
example :: (HasCallStack) => PropertyT IO () -> Property
example = withFrozenCallStack (withTests 1 . property)
