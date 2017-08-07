{- |
Module      : Data.Set.Range.Modify
Description : Basic functions to modify the contents of a range set.
Copyright   : (c) 2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Data.Set.Range.Modify
( insertPoint
, insertRange
, removePoint
, removeRange
) where

import Data.Set.Range.Combine
import Data.Set.Range.Types


-- | Insert a single point into the range set.
insertPoint :: (Ord a, Enum a)
  => a          -- ^ point
  -> RangeSet a -- ^ old range set
  -> RangeSet a -- ^ new range set
insertPoint p = union [(p,p)]

-- | Insert a range into the range set.
insertRange :: (Ord a, Enum a)
  => (a,a)      -- ^ range
  -> RangeSet a -- ^ old range set
  -> RangeSet a -- ^ new range set
insertRange r = union [r]

-- | Remove a single point from the range set.
removePoint :: (Ord a, Enum a)
  => a
  -> RangeSet a
  -> RangeSet a
removePoint p = flip difference [(p,p)]

-- | Remove a range from the range set.
removeRange :: (Ord a, Enum a)
  => (a,a)
  -> RangeSet a
  -> RangeSet a
removeRange r = flip difference [r]
