module Data.Set.Range.Overlap
( Overlap(..)
, overlap
) where

-- | Comparison of two ranges.
data Overlap
 = Equal      -- ^ ranges are equal
 | FstSmaller -- ^ no overlap + first range is smaller
 | FstInside  -- ^ first range is inside the second one
 | FstOverlap -- ^ first range is smaller and overlaps with second one
 | SndSmaller -- ^ no overlap + second range is smaller
 | SndInside  -- ^ second range is inside the first one
 | SndOverlap -- ^ second range is smaller and overlaps with first one
 deriving (Eq, Show)

-- | Compute the overlap relationship between to ranges.
overlap :: Ord a
        => (a,a)   -- ^ first range
        -> (a,a)   -- ^ second range
        -> Overlap -- ^ overap relationship
overlap (a,b) (c,d)
  | a == c         && b == d         = Equal
  | a < c && b < c && a < d && b < d = FstSmaller
  | between a c d  && between b c d  = FstInside
  | a < c          && between b c d  = FstOverlap
  | c < a && d < b && c < b && d < a = SndSmaller
  | between c a b  && between d a b  = SndInside
  | c < a          && between d a b  = SndOverlap
  | otherwise                        = Equal -- dead code
  where
    between x lo hi = lo <= x && x <= hi
