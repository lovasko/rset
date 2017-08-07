import Data.List
import Data.Word
import Safe
import System.Environment
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Data.Set.Range as R


-- | Test the range set membership.
queryPointTest :: [Word8]
               -> Word8
               -> Bool
queryPointTest xs y = rset == set
  where
    rset = R.queryPoint y (R.fromList xs)
    set  = elem y xs

-- | Test the range set membership.
queryRangeTest :: [Word8]       -- ^ points
               -> (Word8,Word8) -- ^ range
               -> Bool          -- ^ result
queryRangeTest xs (a,b) = rset == set
  where
    lo   = min a b
    hi   = max a b
    rset = R.queryRange (lo,hi) (R.fromList xs)
    set  = all (flip elem xs) [lo .. hi]

-- | Test the empty set checking.
nullTest :: [Word8] -- ^ points
         -> Bool    -- ^ result
nullTest xs = rset == set
  where
    rset = R.null (R.fromList xs)
    set  = null xs

-- | Test the conversion from and to lists.
listTest :: [Word8] -- ^ points
         -> Bool    -- ^ result
listTest xs = (sort . nub) xs == (R.toList . R.fromList) xs

-- | Test the conversion from and to ascending lists.
ascListTest :: [Word8] -- ^ points
            -> Bool    -- ^ result
ascListTest xs = R.fromList xs == (R.fromAscList . sort) xs

-- | Test the conversion from and to descending lists.
descListTest :: [Word8] -- ^ points
             -> Bool    -- ^ result
descListTest xs = R.fromList xs == (R.fromDescList . sortBy (flip compare)) xs

insertPointTest :: [Word8]
                -> Word8
                -> Bool
insertPointTest xs y = rset == set
  where
    rset = R.toList $ R.insertPoint y (R.fromList xs)
    set  = sort $ nub (y:xs)

insertRangeTest :: [Word8]
                -> (Word8,Word8)
                -> Bool
insertRangeTest xs (a,b) = rset == set
  where
    rset = R.toList $ R.insertRange (a',b') (R.fromList xs)
    set  = sort $ nub ([a' .. b'] ++ xs)
    a'   = min a b
    b'   = max a b

-- | Test removing a single point from the range set.
removePointTest :: [Word8] -- ^ points
                -> Word8   -- ^ point to remove
                -> Bool    -- ^ result
removePointTest xs y = rset == set
  where
    rset = R.toList $ R.removePoint y (R.fromList xs)
    set  = sort $ nub $ filter (/= y) xs

-- | Test removing a range from the range set.
removeRangeTest :: [Word8]
                -> (Word8,Word8)
                -> Bool
removeRangeTest xs (a,b) = rset == set
  where
    lo   = min a b
    hi   = max a b
    rset = R.toList $ R.removeRange (lo,hi) (R.fromList xs)
    set  = sort $ nub $ filter (not . flip elem [lo .. hi]) xs

-- | Test the union of two sets.
unionTest :: [Word8] -- ^ points
          -> [Word8] -- ^ points
          -> Bool    -- ^ result
unionTest xs ys = rset == set
  where
    rset = R.toList $ R.union (R.fromList xs) (R.fromList ys)
    set  = sort $ nub $ union xs ys

-- | Test the intersection of two sets.
intersectTest :: [Word8] -- ^ points
              -> [Word8] -- ^ points
              -> Bool    -- ^ result
intersectTest xs ys = rset == set
  where
    rset = R.toList $ R.intersect (R.fromList xs) (R.fromList ys)
    set  = sort $ nub $ intersect xs ys

-- | Test the diffence of one set from another.
differenceTest :: [Word8] -- ^ points
               -> [Word8] -- ^ points
               -> Bool    -- ^ result
differenceTest xs ys = rset == set
  where
    rset = R.toList $ R.difference (R.fromList xs) (R.fromList ys)
    set  = sort $ nub $ filter (not . flip elem ys) xs

-- | Test the number of stored points in a set.
sizeTest :: [Word8]
         -> Bool
sizeTest xs = rset == set
  where
    rset = (R.size . R.fromList) xs :: Integer
    set  = (genericLength . nub) xs :: Integer

-- | Print a name of the property test and execute the QuickCheck
-- algorithm.
runTest :: Args               -- ^ property test settings
        -> (String, Property) -- ^ name & test
        -> IO Result          -- ^ result
runTest args (name, prop) = do
  result <- quickCheckWithResult args prop
  putStr $ unwords [name, output result]
  return result

-- | Run all available property tests and collect results.
runTests :: Args
         -> IO [Result]
runTests args = mapM (runTest args) tests
  where tests = [ ("null       ", property nullTest)
                , ("list       ", property listTest)
                , ("ascList    ", property ascListTest)
                , ("descList   ", property descListTest)
                , ("queryPoint ", property queryPointTest)
                , ("queryRange ", property queryRangeTest)
                , ("insertPoint", property insertPointTest)
                , ("insertRange", property insertRangeTest)
                , ("removePoint", property removePointTest)
                , ("removeRange", property removeRangeTest)
                , ("union      ", property unionTest)
                , ("intersect  ", property intersectTest)
                , ("difference ", property differenceTest)
                , ("size       ", property sizeTest) ]

-- | Parse command-line options into test arguments. In case invalid or
-- no arguments were provided, the test fallbacks into a default value.
parseArguments :: [String]
               -> Args
parseArguments []    = stdArgs { maxSuccess=20000,           chatty=False }
parseArguments (x:_) = stdArgs { maxSuccess=readDef 20000 x, chatty=False }

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn "\nRunning property tests:"
  args    <- getArgs
  results <- runTests (parseArguments args)
  if all isSuccess results then exitSuccess else exitFailure
