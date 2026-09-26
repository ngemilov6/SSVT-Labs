<<<<<<< HEAD
import Prelude

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = even x && x > 3
prop2 x = even x
prop3 x = even x || x > 3
prop4 x = (even x && x > 3) || even x

domain :: [Int]
domain = [-10 .. 10]

main :: IO ()
main = do
    putStrLn "Comparing property strengths...\n"
    
    putStrLn $ "Is prop1 stronger than prop2? " ++ show (stronger domain prop1 prop2)
    putStrLn $ "Is prop2 stronger than prop3? " ++ show (stronger domain prop2 prop3)
    putStrLn $ "Is prop4 identical in strength to prop2? " ++ 
               show (stronger domain prop2 prop4 && stronger domain prop4 prop2)
    
    putStrLn "\n--- Descending Strength List ---"
    putStrLn "1. prop1 (Strongest)"
    putStrLn "2. prop2 & prop4 (Equivalent level)"
    putStrLn "3. prop3 (Weakest)"
=======
module Exercise3 where

import Data.List
import Test.QuickCheck

infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- Base relations provided by the assignment
stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs
weaker xs p q = stronger xs q p

-- Derived relational operators for precise sorting
strictlyStronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyStronger xs p q = stronger xs p q && not (stronger xs q p)

strictlyWeaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictlyWeaker xs p q = strictlyStronger xs q p

equallyStrong :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equallyStrong xs p q = stronger xs p q && stronger xs q p

notComparable :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
notComparable xs p q = not (stronger xs p q) && not (stronger xs q p)

-- Custom data type to bind a property's logic to its printable name
data Prop = Prop String (Int -> Bool)
instance Show Prop where 
    show (Prop name _) = name

-- The four properties from Workshop 2, Exercise 3
prop_even :: Prop
prop_even = Prop "even" even

prop_even_g3 :: Prop
prop_even_g3 = Prop "even and >3" (\n -> even n && n > 3)

prop_even_og3 :: Prop
prop_even_og3 = Prop "even or >3" (\n -> even n || n > 3)

prop_even_g3_oeven :: Prop
prop_even_g3_oeven = Prop "(even and >3) or even" (\n -> (even n && n > 3) || even n)

-- Automated ranking algorithm adapted from QuickSort
-- Groups equally strong properties together in sublists
quickRank :: [Int] -> [Prop] -> [[Prop]]
quickRank _ [] = []
quickRank xs (pivot@(Prop _ q):ps) = 
    quickRank xs (filter (\(Prop _ p) -> strictlyStronger xs q p) ps)
    ++ [pivot : filter (\(Prop _ p) -> equallyStrong xs q p) ps]
    ++ quickRank xs (filter (\(Prop _ p) -> strictlyWeaker xs q p) ps)

-- QuickCheck property to verify the distribution of even/odd numbers in our tests
test_even_p :: Int -> Property
test_even_p n = 
    classify (even n) "Even" $
    classify (odd n) "Odd" $
    property True

main :: IO ()
main = do
    putStrLn "--- Testing Property Distribution ---"
    quickCheck test_even_p
    
    putStrLn "\n--- Descending Strength List ---"
    let domain = [(-10)..10]
    let ranking = quickRank domain [prop_even, prop_even_g3, prop_even_og3, prop_even_g3_oeven]
    
    -- Print the ranked equivalence classes
    mapM_ print ranking
>>>>>>> 8f3672db976597164759f33a44254c1934725ae4
