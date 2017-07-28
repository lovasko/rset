import Data.List
import Data.Word
import Safe
import System.Environment
import System.Exit
import Test.QuickCheck
import Test.QuickCheck.Test

import qualified Data.Set.Range as R


-- | Test the range set membership.
queryTest :: [Word8]       -- ^ points
          -> (Word8,Word8) -- ^ range
          -> Bool          -- ^ result
queryTest xs (a,b) = rset == set
  where
    lo   = min a b
    hi   = max a b
    rset = R.queryRange (lo,hi) (R.fromList xs)
    set  = all (flip elem xs) [lo .. hi]

-- | Test the conversion from and to lists.
listTest :: [Word8]  -- ^ points
         -> Bool     -- ^ result
listTest xs = (sort . nub) xs == (R.toList . R.fromList) xs

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
  where tests = [ ("list      ", property listTest)
                , ("query     ", property queryTest)
                , ("union     ", property unionTest)
                , ("intersect ", property intersectTest)
                , ("difference", property differenceTest)
                , ("size      ", property sizeTest) ]

-- | Parse command-line options into test arguments. In case invalid or
-- no arguments were provided, the test fallbacks into a default value.
parseArguments :: [String]
               -> Args
parseArguments []    = stdArgs { maxSuccess=100000,           chatty=False }
parseArguments (x:_) = stdArgs { maxSuccess=readDef 100000 x, chatty=False }

-- | Evaluate test results and set appropriate process exit code.
main :: IO ()
main = do
  putStrLn "\nRunning property tests:"
  args    <- getArgs
  results <- runTests (parseArguments args)
  if all isSuccess results then exitSuccess else exitFailure
