module Data.Set.Range.General
( empty
, fromList
, null
, size
, toList
) where

import qualified Data.List as L
import Prelude hiding (null)

import Data.Set.Range.Access
import Data.Set.Range.Types


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
toList []           = []
toList ((a,b) : xs) = [a .. b] ++ toList xs
