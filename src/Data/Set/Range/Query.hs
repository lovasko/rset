module Data.Set.Range.Query
( queryPoint
, queryRange
) where

import Data.Set.Range.Overlap
import Data.Set.Range.Types


-- | Test whether a point is included in the range set.
queryPoint :: Ord a
           => a          -- ^ point
           -> RangeSet a -- ^ range
           -> Bool       -- ^ decision
queryPoint p = queryRange (p,p)

-- | Test whether a range is included in the range set.
queryRange :: Ord a
           => (a,a)      -- ^ range
           -> RangeSet a -- ^ range set
           -> Bool       -- ^ decision
queryRange _ []       = False
queryRange x (r : rs) = go $ overlap x r
  where
    go Equal      = True
    go FstSmaller = False
    go FstInside  = True
    go FstOverlap = False
    go SndSmaller = queryRange x rs
    go SndInside  = False
    go SndOverlap = False
