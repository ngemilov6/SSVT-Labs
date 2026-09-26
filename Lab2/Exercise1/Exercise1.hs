import System.Random
import Data.List (nub, sort)
import SetOrd
import Test.QuickCheck

genRandomSet :: IO (Set Int)
genRandomSet = do
    gen <- newStdGen
    let (size, gen') = randomR (0, 20 :: Int) gen
        rawInts = take size (randomRs (-100, 100) gen')
    return $ Set (sort (nub rawInts))

instance (Ord a, Arbitrary a) => Arbitrary (Set a) 
    where arbitrary = do
        xs <- arbitrary
        return $ Set (sort (nub xs))

