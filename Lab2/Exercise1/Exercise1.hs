module Exercise1 where

<<<<<<< HEAD
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
=======
import Test.QuickCheck
import SetOrd
import System.Random
import Data.List

-- run with:
-- runghc --ghc-arg=-i.. Exercise1.hs +RTS -M512M -RTS

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

>>>>>>> ed68974973bb76b622917150b9706beb21100f3b
