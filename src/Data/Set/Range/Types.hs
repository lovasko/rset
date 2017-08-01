module Data.Set.Range.Types
( Range
, RangeSet
) where

-- | A simple range, denoted by the low and high boundaries.
type Range a = (a,a)

-- | A set of ranges
type RangeSet a = [Range a]
