module Exercise1 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

-- mplement a random data generator for the datatype Set Int , where Set is as
-- defined in SetOrd.hs. First do this from scratch, next give a version that usesQuickCheck to random test this datatype.

genSet :: Int -> IO (Set Int)
genSet n = do
    xs <- sequence $ replicate n (randomIO :: IO Int)
    return (list2set xs)

genSetQuickCheck :: Gen (Set Int)
genSetQuickCheck = list2set <$> listOf arbitrary

instance Arbitrary (Set Int) where
    arbitrary = genSetQuickCheck    

prop_duplicateFree :: Property
prop_duplicateFree = forAll genSetQuickCheck  (\(Set x) -> length (x) == length (nub (x)))

main :: IO ()
main = do
    putStrLn "Generating a random Set Int with 10 elements:"
    randomSet <- genSet 10
    print randomSet
    putStrLn "Running QuickCheck tests for duplicate-free property:"
    quickCheck prop_duplicateFree