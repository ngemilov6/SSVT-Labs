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