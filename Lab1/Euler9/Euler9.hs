module Euler9 where

import Test.QuickCheck

-- We abstract the target sum to allow testing of the underlying mathematical logic.
findTriplets :: Integer -> [(Integer, Integer, Integer)]
findTriplets target = 
    [ (a, b, c) |
      a <- [1 .. target `div` 3],
      b <- [a + 1 .. target `div` 2],
      let c = target - a - b,
      a^2 + b^2 == c^2
    ]

-- Random integers rarely yield valid Pythagorean triplets, 
-- so filtering them would cause QuickCheck 
-- to abandon the test due to too many discards. 
-- Instead, we use Euclid's formula (a = m^2 - n^2, b = 2mn, c = m^2 + n^2) 
-- to generate guaranteed valid target sums.
genValidTripletSum :: Gen Integer
genValidTripletSum = do
    m <- choose (2, 50)
    n <- choose (1, m - 1)
    let a = m^2 - n^2
        b = 2 * m * n
        c = m^2 + n^2
    return (a + b + c)

-- property to test if the sum of the tuple elements equals the target input
prop_validSum :: Property
prop_validSum = forAll genValidTripletSum $ \target ->
    let triplets = findTriplets target
    in all (\(a, b, c) -> a + b + c == target) triplets

-- property to test if the tuple strictly satisfies the Pythagorean theorem.
prop_validPythagorean :: Property
prop_validPythagorean = forAll genValidTripletSum $ \target ->
    let triplets = findTriplets target
    in all (\(a, b, c) -> a^2 + b^2 == c^2) triplets

-- property to test if the strict inequality constraint a < b < c is maintained.
prop_strictOrdering :: Property
prop_strictOrdering = forAll genValidTripletSum $ \target ->
    let triplets = findTriplets target
    in all (\(a, b, c) -> a < b && b < c) triplets

main :: IO ()
main = do
    putStrLn "--- Testing Sum Constraint ---"
    quickCheck prop_validSum
    
    putStrLn "--- Testing Pythagorean Constraint ---"
    quickCheck prop_validPythagorean
    
    putStrLn "--- Testing Strict Ordering (a < b < c) ---"
    quickCheck prop_strictOrdering
    
    putStrLn "\nEuler 9 Output for target 1000:"
    let [(a, b, c)] = findTriplets 1000
    putStrLn $ "The triplet is: " ++ show (a, b, c)
    putStrLn $ "The product is: " ++ show (a * b * c)