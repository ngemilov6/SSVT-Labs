{-# LANGUAGE NoExplicitForAll #-}

module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1
-- import Lecture2
-- import Lecture3


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- forall :: [a] -> (a -> Bool) -> Bool
-- forall = flip all

--- genSmallNat used in Exercise 1
genSmallNat :: Gen Integer
genSmallNat = chooseInteger (1, 25)

genSmallList :: Gen [Integer]
genSmallList = do
    k <- genSmallNat
    return [1..k]

powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = map (x:) pxs ++ pxs where pxs = powerset xs

prop_cardinality_pset :: Property
prop_cardinality_pset = forAll genSmallList (\ns -> 2^(length ns) == length (powerset ns))

main :: IO()
main = do
    quickCheck prop_cardinality_pset
    input <- getLine
    let l = read input :: [Int]
    print (powerset l)


{- Time taken: 30min
1. Testing becomes hard, as allowing list sizes of more than 25 leads to testing taking an unreasonable amount of time.
That is why restrictions on the input for the property test have to be very strict, making it harder to draw conclusions.
The exponential blow up of combinations, which is exactly what we want to test, is leading to tests not being computable in reasonable amounts of time.
2. When I am computing these tests, I am testing with the aim of showing that the function works as expected for any input.
However, due to the difficulties, I am really only showing that it works for a very limited set of the input.
The property can be mathematically proven, as we have done in the workshop, so the tests are not in order to see whether a property holds for a sample of the input.
Test report:
Tests for the cardinality all pass relatively quickly with restricition of max length of 25 for the input list. 
For larger input lists, the test takes a long time, as computing the powerset is very demanding.
-}