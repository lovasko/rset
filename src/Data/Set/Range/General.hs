module Data.Set.Range.General
( empty
, null
, size
) where

import qualified Data.List as L
import Prelude hiding (null)

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
