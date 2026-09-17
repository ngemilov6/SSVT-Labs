import Prelude
import Test.QuickCheck ( quickCheck, NonNegative(NonNegative), Gen, Property, forAll, chooseInteger )
import Text.Printf (errorBadArgument)
import Criterion.Main ---Additional import for Benchmarking

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--- Recursive definition of factorial.
factorial :: Integer -> Integer
factorial n | n > 0 = n * factorial (n - 1)
            | n == 0 = 1 
            | otherwise = error "Bad argument"

--- Helper to generate the list [1..n] with special case for 0 = [1].
listReference :: Integer -> [Integer]
listReference 0 = [1]
listReference n = [1..n]

--- Definition of factorial using foldr, also applying a recursive multiplication procedure.
factorialReference :: Integer -> Integer
factorialReference n 
    | n >= 0 = foldr (*) 1 (listReference n)
    | otherwise = error "Bad argument"
                    

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 20)

--- Properties using only factorial function itself to self-confirm
prop_factorialPositive :: Property
prop_factorialPositive = 
    forAll genSmallNat (\n -> factorial n >= 1)

prop_factorialRecursive :: Property
prop_factorialRecursive = 
    forAll genSmallNat (\n -> factorial (n + 1) == (n + 1) * factorial n)

prop_factorialIncreasing :: Property
prop_factorialIncreasing = 
    forAll genSmallNat (\n -> n /= 0 --> factorial (n + 1) > factorial n)

--- Properties using reference function to verify factorial
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

    putStrLn "\nRunning Benchmarks"
    defaultMain [
      bgroup "Factorial of 1000" [
        bench "Recursive factorial" $ whnf factorial 1000,
        bench "Fold factorial"  $ whnf factorialReference 1000
        ]]
    input <- getLine
    let n = read input :: Integer
    putStrLn ((show n) ++ "! = " ++ show (factorial n))