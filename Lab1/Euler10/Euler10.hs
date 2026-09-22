module Euler10 where

import Test.QuickCheck

primes :: [Integer]
primes = 2 : 3 : filter isPrime [5,7..]
  where
    isPrime n = foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

-- helper function to allow testing
sumPrimesBelow :: Integer -> Integer
sumPrimesBelow limit = sum $ takeWhile (< limit) primes

euler10 :: Integer
euler10 = sumPrimesBelow 2000000

-- naive O(n) checker to independently verify our optimized generator
naiveIsPrime :: Integer -> Bool
naiveIsPrime n 
    | n < 2     = False
    | otherwise = all (\x -> n `rem` x /= 0) [2..(n-1)]

-- generate a random limit for our sum function, kept relatively small to keep testing fast
genSmallLimit :: Gen Integer
genSmallLimit = choose (10, 10000)

-- generate a random index to pluck a prime from our lazy list
genPrimeIndex :: Gen Int
genPrimeIndex = choose (0, 1000)


-- property to verify that the optimized generator only produces true primes
prop_isActuallyPrime :: Property
prop_isActuallyPrime = forAll genPrimeIndex $ \index ->
    let p = primes !! index
    in naiveIsPrime p

-- property to test the sum of primes below X should never exceed the sum of primes below Y if X <= Y.
prop_monotonicSum :: Property
prop_monotonicSum = forAll genSmallLimit $ \limit1 ->
    forAll genSmallLimit $ \limit2 ->
        let x = min limit1 limit2
            y = max limit1 limit2
        in sumPrimesBelow x <= sumPrimesBelow y

-- proprty to test against known mathematical constants.
prop_knownBaseCases :: Bool
prop_knownBaseCases = 
    sumPrimesBelow 10 == 17 && -- 2 + 3 + 5 + 7
    sumPrimesBelow 20 == 77    -- 17 + 11 + 13 + 17 + 19

main :: IO ()
main = do
    putStrLn "--- Testing Optimized Prime Generator against Naive Checker ---"
    quickCheck prop_isActuallyPrime

    putStrLn "--- Testing Sum Monotonicity ---"
    quickCheck prop_monotonicSum

    putStrLn "--- Testing Known Base Cases ---"
    print prop_knownBaseCases
    
    putStrLn "\nEuler 10 Output (Sum of primes below 2,000,000):"
    print euler10