{-# LANGUAGE NoExplicitForAll #-}

module Exercise4 where

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

--- Helper function for isPermutation to check for equal length
equal_length :: [a] -> [a] -> Bool
equal_length l1 l2 = length l1 == length l2

--- Helper function for isPermutation to count occurences of an element in a list
count_occurs :: Eq a => a -> [a] -> Int
count_occurs n xs = length (filter (== n) xs)

--- Helper function for isPermutation to check whether an element occurs the same amount of times in two lists
same_count :: Eq a => [a] -> [a] -> a -> Bool
same_count xs ys n = count_occurs n xs == count_occurs n ys

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation l1 l2 = equal_length l1 l2 && (all (\x -> same_count l1 l2 x) l1)

--- Generate a random number, adjusted from previous exercises and lecture material
genSmallNat :: Int -> Int -> Gen Int
genSmallNat a b = chooseInt (a, b)

--- Generates a list of n random input numbers between 1 and and supplied maximum Value
genList :: Int -> Int -> Gen [Int]
genList 0 _ = pure []
genList n maxVal = do
    x <- genSmallNat 1 maxVal
    xs <- genList (n-1) maxVal
    pure (x:xs)

--- Generates a permutation from input permutation
makePerm :: [Int] -> Gen [Int]
makePerm [] = pure []
makePerm xs = do
    k <- genSmallNat 0 ((length xs)-1)
    let (ys,zs) = splitAt k xs
        rest = ys ++ (tail zs)
        select = head zs
    remaining <- makePerm rest
    pure (select:remaining)

--- Generator for permutations, based on a random length within bounds and a random maximum value within bounds.
--- An initial permutation is generated, and from that, a new permutation.
gen_perms :: Gen [[Int]]
gen_perms = do
    length_perm <- genSmallNat 3 10
    max_val <- genSmallNat 1 100
    init_perm <- genList length_perm max_val
    perm <- makePerm init_perm
    pure [init_perm, perm]

--- Properties to test isPermutation
prop_equal_length :: Property
prop_equal_length = forAll gen_perms (\perm -> equal_length (perm !! 0) (perm !! 1))

prop_same_count :: Property
prop_same_count = forAll gen_perms (\perm -> all (\x -> same_count (perm !! 0) (perm !! 1) x) (perm !! 0))

main :: IO()
main = do
    print (isPermutation [1,2,4] [4,2,4])
    print (isPermutation [1,2,4,5] [1,2,4])
    print (isPermutation [1,2,3,4] [1,2,3,4])
    print (isPermutation [1,2,3,4] [1,2,4,3])
    print (isPermutation ["a"] ["a"])
    quickCheck prop_equal_length
    quickCheck prop_same_count

{- Time taken: 110min
I implemented several helper functions to check for two lists having the same length, which is a requirement for a permutation,
for a number to occur the same amount of times in two lists, and a helper function to count occurences of an element in a list.
I then defined a permutation as a list that satisfies the equal length and same count for all elements in the first list.
The equal length is technically also included in the same count, but then the count would have to be performed both ways.

For testing I created a set of functions to automatically generate permutations.
First, I took the function to generate a random small integer and adjusted it to include custom bounds.
I then made a list generator, which recursively generates a list of specified length within value bounds.
From this I made a function that computes a permutation of an exisiting list by 
recursively picking a random element as the next element in the list.
Finally, I made a function which puts it all together and serves quickCheck to initialize an initial list and permutation thereof
for testing.
I tested two properties, the length being the same, and that all elements from one list, are also present in the other with the same frequency.

-}