<<<<<<< HEAD
import Prelude
import Test.QuickCheck ( quickCheck, NonNegative(NonNegative), Gen, Property, forAll, chooseInteger )
import Text.Printf (errorBadArgument)
import Criterion.Main


factorial :: Integer -> Integer
factorial n | n > 1 = n * factorial (n - 1)
            | n == 1 = 1
            | n == 0 = 1 
            | otherwise = error "Bad argument"

=======
module Exercise1 where

import Prelude
import Test.QuickCheck ( quickCheck, NonNegative(NonNegative), Gen, Property, forAll, chooseInteger )
import Text.Printf (errorBadArgument)

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--- Recursive definition of factorial.
factorial :: Integer -> Integer
factorial n | n > 0 = n * factorial (n - 1)
            | n == 0 = 1 
            | otherwise = error "Bad argument"

--- Helper to generate the list [1..n] with special case for 0 = [1].
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4
listReference :: Integer -> [Integer]
listReference 0 = [1]
listReference n = [1..n]

<<<<<<< HEAD
factorialReference :: Integer -> Integer
factorialReference n = foldr (*) 1 (listReference n)
=======
--- Definition of factorial using foldr, also applying a recursive multiplication procedure.
factorialReference :: Integer -> Integer
factorialReference n 
    | n >= 0 = foldr (*) 1 (listReference n)
    | otherwise = error "Bad argument"
                    
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 20)

<<<<<<< HEAD
=======
--- Properties using only factorial function itself to self-confirm
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4
prop_factorialPositive :: Property
prop_factorialPositive = 
    forAll genSmallNat (\n -> factorial n >= 1)

<<<<<<< HEAD
=======
prop_factorialRecursive :: Property
prop_factorialRecursive = 
    forAll genSmallNat (\n -> factorial (n + 1) == (n + 1) * factorial n)

prop_factorialIncreasing :: Property
prop_factorialIncreasing = 
    forAll genSmallNat (\n -> n /= 0 --> factorial (n + 1) > factorial n)

--- Properties using reference function to verify factorial
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4
prop_factorial_matches_reference_1 :: Property
prop_factorial_matches_reference_1 = 
    forAll genSmallNat (\n -> factorial n == factorialReference n)

prop_factorial_matches_reference_2 :: NonNegative Integer -> Bool
prop_factorial_matches_reference_2 (NonNegative n) = 
    factorial n == factorialReference n

main ::  IO ()
main = do
    putStrLn "Running QuickCheck Properties"
    quickCheck prop_factorialPositive
    quickCheck prop_factorial_matches_reference_1
    quickCheck prop_factorial_matches_reference_2

<<<<<<< HEAD
    putStrLn "\nRunning Benchmarks"
    defaultMain [
      bgroup "Factorial of 1000" [
        bench "Recursive factorial" $ whnf factorial 1000,
        bench "Fold factorial"  $ whnf factorialReference 1000
        ]]
=======
    input <- getLine
    let n = read input :: Integer
    putStrLn ((show n) ++ "! = " ++ show (factorial n))
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4
