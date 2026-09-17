{-# LANGUAGE NoExplicitForAll #-}

module Exercise1 where

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

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n*(factorial (n-1))

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 50)

prop_factorialPositive :: Property
prop_factorialPositive = forAll genSmallNat (\n -> factorial n >= 1)

prop_factorialRecursive :: Property
prop_factorialRecursive = forAll genSmallNat (\n -> factorial (n + 1) == (n + 1) * factorial n)

prop_factorialIncreasing :: Property
prop_factorialIncreasing = forAll genSmallNat (\n -> n /= 0 --> factorial (n + 1) > factorial n)

main :: IO()
main = do
    quickCheck prop_factorialPositive
    quickCheck prop_factorialRecursive
    quickCheck prop_factorialIncreasing
    input <- getLine
    let n = read input :: Integer
    print (factorial n)


{- Time taken: 30min
The factorial function is defined recursively, with the base case being factorial 0 = 1. 
The properties defined for testing the factorial function include checking that the result is always positive (given in the task), 
that the recursive definition holds, and that the function is increasing for positive integers.
For the last it was necessary to include the precondition that n is not equal to 0, since factorial(0) = 1 and factorial(1) = 1, which violates the property for this specific case.
The small natural number generator was also taken from the example, but extended to a slightly bigger range.
-}