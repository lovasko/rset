{- |
Module      : Data.Set.Range
Description : Data structure that stores a set of ranges of types that
              implement Eq, Ord and Enum typeclasses.
Copyright   : (c) 2017 Daniel Lovasko
License     : BSD2

Maintainer  : Daniel Lovasko <daniel.lovasko@gmail.com>
Stability   : stable
Portability : portable
-}

module Data.Set.Range
( Range
, RangeSet

-- general
, empty
, null
, size

-- list conversions
, fromAscList
, fromDescList
, fromList
, toList

-- access
, insertPoint
, insertRange
, removePoint
, removeRange

-- member testing
, queryPoint
, queryRange

-- combinations
, difference
, intersect
, union
) where

import Prelude hiding (null)

import Data.Set.Range.Combine
import Data.Set.Range.General
import Data.Set.Range.List
import Data.Set.Range.Modify
import Data.Set.Range.Query
import Data.Set.Range.Types
