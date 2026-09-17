{-# LANGUAGE NoExplicitForAll #-}

module Exercise3 where

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

stronger, weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\ x -> p x --> q x) xs
weaker xs p q = stronger xs q p

--- Additional propertis to create the ranking
strictly_stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictly_stronger xs p q = stronger xs p q && not(stronger xs q p)

strictly_weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
strictly_weaker xs p q = strictly_stronger xs q p

equally_strong :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
equally_strong xs p q = stronger xs p q && stronger xs q p

not_comparible :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
not_comparible xs p q = not (stronger xs p q) && not (stronger xs q p)

--- Test function to working with classify in order to see percentages of different inputs rather than ending at first counter example.
test_even_p :: Int -> Property
test_even_p n = 
    classify (even n) "Even" $
    classify (odd n) "Odd" $
    property True

--- For each property, the property itself as the test and then the property definition which includes the name for printing.
test_even :: Int -> Bool
test_even n = even n
prop_even = Prop "Even" test_even

test_even_g3 :: Int -> Bool
test_even_g3 n = even n && n>3
prop_even_g3 = Prop "Even and >3" test_even_g3

test_even_og3 :: Int -> Bool
test_even_og3 n = even n || n>3
prop_even_og3 = Prop "Even or >3" test_even_og3

test_even_g3_oeven :: Int -> Bool
test_even_g3_oeven n = (even n && n>3) || even n
prop_even_g3_oeven = Prop "(Even and >3) or even" test_even_g3_oeven


--- Adoption of QuickSort for ranking the properties
--- Adjusted further to be able to work with the new property which includes the name and can be printed
quickRank :: [Int] -> [Prop] -> [[Prop]]
quickRank xs [] = []
quickRank xs (pivot@(Prop name q):ps) = 
    quickRank xs (filter (\(Prop _ p) -> strictly_stronger xs q p) ps)
    ++ [pivot: (filter (\(Prop _ p) -> equally_strong xs q p) ps)]
    ++ quickRank xs (filter (\(Prop _ p) -> strictly_weaker xs q p) ps)

--- Additional definitions to add a string to the property for printing
data Prop = Prop String (Int -> Bool)
instance Show Prop where show (Prop name _) = name

--- Adjusted tests from the Workshop material
-- test1 = stronger [(-10)..10] (\ x -> even x && x > 3) even
-- test2 = stronger [(-10)..10] (\ x -> even x || x > 3) even
-- test3 = stronger [(-10)..10] (\ x -> (even x && x > 3) || even x) even
-- test4 = stronger [(-10)..10] even (\ x -> (even x && x > 3) || even x)

main :: IO()
main = do
    -- quickCheck prop_cardinality_pset
    -- input <- getLine
    -- let l = read input :: [Int]
    -- print (powerset l)
    quickCheck test_even_p
    let ranking = quickRank [(-10)..10] [prop_even, prop_even_g3, prop_even_og3, prop_even_g3_oeven]
    print ranking


{- Time taken: 90min
2. Descending strength list of properties
 - even and >3
 - even; (even and >3) or even
 - even or >3

 even and >3 is at the highest level, as any number that has this property also has all the other properties listed.
 The two properties "even" and "(even and >3) or even" are placed at the same level, as within the second property even dominates.
 This is not to be understood with two separate paths in a Hasse diagram, as they represent the same property, not incomparable properties.
 There is no element that is "even and >3" that is not "even", therefore both properties express the property "even".
 Even or >3 is the weakest property, as it includes all elements that satisfy the other properties, but in addition it also includes odd elemnts >3.

I adjusted the design after the initial construction to include the name in the property. This is so that I could include the Show function for these properties to be printed.
In order to do this, I had to adjust the quicksort mechanism to split and only consider the actual Int -> Bool part of the property for analysis of strength.
-}