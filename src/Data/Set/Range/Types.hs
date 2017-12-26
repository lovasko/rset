{- |
Module      : Data.Set.Range.Types
Description : Types that represent ranges and range sets.
Copyright   : (c) 2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Data.Set.Range.Types
( Range
, RangeSet
) where

-- | A simple range, denoted by the low and high boundaries.
type Range a = (a,a)

-- | A set of ranges.
type RangeSet a = [Range a]
