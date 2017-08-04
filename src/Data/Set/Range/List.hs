module Data.Set.Range.List
( fromAscList
, fromDescList
, fromList
, toList
) where

import Data.Set.Range.General
import Data.Set.Range.Modify
import Data.Set.Range.Types


-- | Create a range set from a list of points. The ordering of the points is
-- not important. The list can contain duplicates.
fromList :: (Ord a, Enum a)
         => [a]        -- ^ list of points
         -> RangeSet a -- ^ range set
fromList = foldr insertPoint empty

-- | Create a range set from a list of ascending points. The list can contain
-- duplicates.
fromAscList :: (Ord a, Enum a)
            => [a]        -- ^ list of ascending points
            -> RangeSet a -- ^ range set
fromAscList = foldr combine []
  where
    combine p []              = [(p,p)]
    combine p ((a,b) : xs)
      | p < a  && succ p == a = (p,b) : xs
      | b < p  && succ b == p = (a,p) : xs
      | a <= p && p <= b      = (a,b) : xs
      | otherwise             = (p,p) : (a,b) : xs

-- | Create a range set from a list of descending points. The list can contain
-- duplicates.
fromDescList :: (Ord a, Enum a)
             => [a]        -- ^ list of ascending points
             -> RangeSet a -- ^ range set
fromDescList = reverse . fromAscList

-- | Convert the range set into a list of points.
toList :: Enum a
       => RangeSet a -- ^ range set
       -> [a]        -- ^ points
toList []           = []
toList ((a,b) : xs) = [a .. b] ++ toList xs
