-- | What a group of this suite is built out of, beside hedgehog's own
-- 'Hedgehog.property': a case that is genuinely one case, and the name for
-- the runs of them a group is written in.
module Havidrome.Check
  ( Checks
  , example
  ) where

import GHC.Stack (HasCallStack, withFrozenCallStack)
import Hedgehog (Property, PropertyName, PropertyT, property, withTests)

-- | A run of properties, named as they will be reported. A group is one or
-- more of these, so that what is being checked is said once, above the
-- checks, rather than at the head of every one of them.
type Checks = [(PropertyName, Property)]

-- | One case, checked once: an example the spec names, or a piece of output
-- pinned exactly as it reads. There is nothing generated in it, so generating
-- it a hundred times would check the same case a hundred times.
example :: (HasCallStack) => PropertyT IO () -> Property
example = withFrozenCallStack (withTests 1 . property)
