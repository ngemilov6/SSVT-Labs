import Prelude
import Test.QuickCheck ( quickCheck, NonNegative(NonNegative), Gen, Property, forAll, chooseInteger )
import Text.Printf (errorBadArgument)
import Criterion.Main


factorial :: Integer -> Integer
factorial n | n > 1 = n * factorial (n - 1)
            | n == 1 = 1
            | n == 0 = 1 
            | otherwise = error "Bad argument"

listReference :: Integer -> [Integer]
listReference 0 = [1]
listReference n = [1..n]

factorialReference :: Integer -> Integer
factorialReference n = foldr (*) 1 (listReference n)

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 20)

prop_factorialPositive :: Property
prop_factorialPositive = 
    forAll genSmallNat (\n -> factorial n >= 1)

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