module Data.Set.Range.Combine
( difference
, intersect
, union
) where

import Data.Set.Range.Overlap
import Data.Set.Range.Types


-- | Merge all adjacent ranges. This function assumes that the range set is
-- sorted by its first range parameter.
merge :: (Eq a, Enum a)
      => RangeSet a -- ^ old range set
      -> RangeSet a -- ^ new range set
merge []        = []
merge [xs]      = [xs]
merge ((a,b) : (c,d) : xs)
  |      b == c =         merge ((a,d) : xs)
  | succ b == c =         merge ((a,d) : xs)
  | otherwise   = (a,b) : merge ((c,d) : xs)

-- | Subtract a range set from another range.
difference :: (Ord a, Enum a)
           => RangeSet a -- ^ first range set
           -> RangeSet a -- ^ second range set
           -> RangeSet a -- ^ difference of two range sets
difference xs           []           = xs
difference []           _            = []
difference ((a,b) : xs) ((c,d) : ys) = go $ overlap (a,b) (c,d)
  where
    go Equal      =              difference xs                ys
    go FstSmaller = (a,b)      : difference xs                ((c,d) : ys)
    go FstInside  =              difference xs                ((c,d) : ys)
    go FstOverlap = (a,pred c) : difference xs                ((succ b,d) : ys)
    go SndSmaller =              difference ((a,b) : xs)      ys
    go SndInside
      | a == c    =              difference ((succ d,b) : xs) ys
      | b == d    = (a,pred c) : difference xs                ys
      | otherwise = (a,pred c) : difference ((succ d,b) : xs) ys
    go SndOverlap =              difference ((succ d,b) : xs) ys

-- | Create an intersection of two range sets.
intersect :: (Ord a, Enum a)
          => RangeSet a -- ^ first range set
          -> RangeSet a -- ^ second range set
          -> RangeSet a -- ^ intersection of two range sets
intersect _  []                     = []
intersect [] _                      = []
intersect ((a,b) : xs) ((c,d) : ys) = merge $ go $ overlap (a,b) (c,d)
  where
    go Equal      = (a,b) : intersect xs           ys
    go FstSmaller =         intersect xs           ((c,d) : ys)
    go FstInside  = (a,b) : intersect xs           ((b,d) : ys)
    go FstOverlap = (c,b) : intersect xs           ((b,d) : ys)
    go SndInside  = (c,d) : intersect ((d,b) : xs) ys
    go SndSmaller =         intersect ((a,b) : xs) ys
    go SndOverlap = (a,d) : intersect ((d,b) : xs) ys

-- | Create an union of two range sets.
union :: (Ord a, Enum a)
      => RangeSet a -- ^ first range set
      -> RangeSet a -- ^ second range set
      -> RangeSet a -- ^ union of two range sets
union xs []                     = xs
union [] ys                     = ys
union ((a,b) : xs) ((c,d) : ys) = merge $ go $ overlap (a,b) (c,d)
  where
    go Equal      = (a,b) : union xs           ys
    go FstSmaller = (a,b) : union xs           ((c,d) : ys)
    go FstInside  =         union xs           ((c,d) : ys)
    go FstOverlap = (a,b) : union xs           ((b,d) : ys)
    go SndSmaller = (c,d) : union ((a,b) : xs) ys
    go SndInside  =         union ((a,b) : xs) ys
    go SndOverlap = (c,d) : union ((d,b) : xs) ys
