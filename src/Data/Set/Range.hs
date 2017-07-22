module Data.Set.Range
( Range
, RangeSet

-- general
, empty
, null
, size

-- conversions
, fromList
, toList

-- access
, insertPoint
, insertRange

-- combinations
, intersect
, union

) where

import qualified Data.List as L
import Prelude hiding (null)


-- | A simple range, denoted by the low and high boundaries.
type Range a = (a, a)

-- | A set of ranges
type RangeSet a = [Range a]

-- | Create an empty range set.
empty :: RangeSet a -- ^ range set
empty = []

-- | Test if the range set does not contain any points.
null :: RangeSet a -- ^ range set
     -> Bool       -- ^ decision
null [] = True
null _  = False

-- | Count the number of unique points stored in the range set.
size :: (Num n, Enum a)
     => RangeSet a -- ^ range set
     -> n          -- ^ number of points
size xs = sum $ map (L.genericLength . uncurry enumFromTo) xs

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

-- | Create a range set from a list of points. The ordering of the points is
-- not important. The list can contain duplicates.
fromList :: (Ord a, Enum a)
         => [a]        -- ^ list of points
         -> RangeSet a -- ^ range set
fromList = foldr insertPoint empty

-- | Convert the range set into a list of points.
toList :: Enum a
       => RangeSet a -- ^ range set
       -> [a]        -- ^ points
toList []         = []
toList ((a,b):xs) = [a .. b] ++ toList xs

-- | Merge all adjacent ranges.
merge :: (Eq a, Enum a)
      => RangeSet a -- ^ old range set 
      -> RangeSet a -- ^ new range set 
merge []        = []
merge [xs]      = [xs]
merge ((a,b) : (c,d) : xs)
  |      b == c =         merge ((a,d) : xs)
  | succ b == c =         merge ((a,d) : xs)
  | otherwise   = (a,b) : merge ((c,d) : xs)
    
data Overlap
 = FstSmaller -- ^ no overlap + first range is smaller
 | SndSmaller -- ^ no overlap + second range is smaller
 | Equal      -- ^ ranges are equal
 | FstInside  -- ^ first range is inside the second one
 | SndInside  -- ^ second range is inside the first one
 | FstOverlap -- ^ first range is smaller and overlaps with second one
 | SndOverlap -- ^ second range is smaller and overlaps with first one
 deriving (Eq, Show)

-- | Compute the overlap relationship between to ranges.
cmp :: Ord a
    => (a,a)   -- ^ first range
    -> (a,a)   -- ^ second range
    -> Overlap -- ^ overap relationship
cmp (a,b) (c,d)
  | a < c         && a < d         = FstSmaller
  | c < a         && c < b         = SndSmaller
  | a == c        && b == d        = Equal
  | between a c d && between b c d = FstInside
  | between c a b && between d a b = SndInside
  | a < c         && between b c d = FstOverlap
  | c < a         && between d a b = SndOverlap
  | otherwise                      = Equal -- ^ dead code
  where
    between x lo hi = lo <= x && x <= hi

-- | Create an union of two range sets.
union :: (Ord a, Enum a)
      => RangeSet a -- ^ first range set
      -> RangeSet a -- ^ second range set
      -> RangeSet a -- ^ union of two range sets
union xs []                 = xs
union [] ys                 = ys
union ((a,b):xs) ((c,d):ys) = merge $ go $ cmp (a,b) (c,d)
  where
    go FstSmaller = (a,b) : union xs ((c,d) : ys)
    go SndSmaller = (c,d) : union ((a,b) : xs) ys
    go Equal      = (a,b) : union xs ys
    go FstInside  =         union xs ((c,d) : ys)
    go SndInside  =         union ((a,b) : xs) ys
    go FstOverlap = (a,b) : union xs ((b,d) : ys)
    go SndOverlap = (c,d) : union ((d,b) : xs) ys

-- | Create an intersection of two range sets.
intersect :: (Ord a, Enum a)
          => RangeSet a -- ^ first range set
          -> RangeSet a -- ^ second range set
          -> RangeSet a -- ^ intersection of two range sets
intersect _  []                 = []
intersect [] _                  = []
intersect ((a,b):xs) ((c,d):ys) = merge $ go $ cmp (a,b) (c,d)
  where
    go FstSmaller =         intersect xs ys
    go SndSmaller =         intersect xs ys
    go Equal      = (a,b) : intersect xs ys
    go FstInside  = (a,b) : intersect xs ((c,a) : (b,d) : ys)
    go SndInside  = (c,d) : intersect ((a,c) : (b,d) : xs) ys
    go FstOverlap = (c,b) : intersect xs ((b,d) : ys)
    go SndOverlap = (a,d) : intersect ((d,b) : xs) ys
