module Exercise1 where

import Test.QuickCheck
import SetOrd
import System.Random
import Data.List

-- Generator from scratch
genSet :: Int -> IO (Set Int)
genSet n = do
    xs <- mapM (const (randomRIO (0, 20))) [1..n]
    return (list2set xs)

-- Generator using QuickCheck
genSetQuickCheck :: Gen (Set Int)
genSetQuickCheck = list2set <$> listOf (choose (0, 20))

instance Arbitrary (Set Int) where
    arbitrary = genSetQuickCheck

-- Property to test that the generated set has no duplicates
prop_set_no_duplicates :: Property
prop_set_no_duplicates = forAll genSetQuickCheck $ \(Set xs) -> length xs == length (nub xs)

-- no other tests since other properties require union, intersection, etc. which are not implemented yet

main ::  IO ()
main = do
    genSet 10 >>= print
    quickCheck prop_set_no_duplicates