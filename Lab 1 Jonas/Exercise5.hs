{-# LANGUAGE NoExplicitForAll #-}

module Exercise5 where

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
    length_perm <- genSmallNat 1 5
    max_val <- genSmallNat 1 10
    init_perm <- genList length_perm max_val
    perm <- makePerm init_perm
    pure [init_perm, perm]



--- isDerangement property defined recursively, using a helper function
sub_isDerangement :: Eq a => [a] -> [a] -> Bool
sub_isDerangement [] [] = True
sub_isDerangement _ [] = False
sub_isDerangement [] _ = False
sub_isDerangement l1 l2 = not (head l1 == head l2) && sub_isDerangement (tail l1) (tail l2)

--- Checks whether the two lists are permutations of each other, and then whether it is a derangement
isDerangement :: (Eq a, Show a) => [a] -> [a] -> Bool
isDerangement l1 l2 = isPermutation l1 l2 && sub_isDerangement l1 l2


--- Generates a list of all possibilities of picking one integer from a list with that integer and the remaining list
pick_single :: [Int] -> [(Int,[Int])]
pick_single [] = []
pick_single (x:xs) = (x, xs) : [(y, x: others) | (y, others) <- pick_single xs]

--- Generates a list of all permutations of list
all_permutations :: [Int] -> [[Int]]
all_permutations [] = [[]]
all_permutations xs = [x:rest_perm | (x,rest) <- pick_single xs, rest_perm <- all_permutations rest]

--- Generates a list of all derangements of [0..(n-1)], my generating permutations and removing those that are not a derangmeent
deran :: Int -> [[Int]]
deran n = [permutation | permutation <- all_permutations [0..(n-1)], isDerangement permutation [0..(n-1)]]

--- Testable Properties for isDerangement
prop_equal_length :: Property
prop_equal_length = forAll gen_perms (\perm -> isDerangement (perm !! 0) (perm !! 1) --> equal_length (perm !! 0) (perm !! 1))

prop_same_count :: Property
prop_same_count = forAll gen_perms (\perm -> isDerangement (perm !! 0) (perm !! 1) --> all (\x -> same_count (perm !! 0) (perm !! 1) x) (perm !! 0))

prop_permutation :: Property
prop_permutation = forAll gen_perms (\perm -> isDerangement (perm !! 0) (perm !! 1) --> isPermutation (perm !! 0) (perm !! 1))

prop_diagonal :: Property
prop_diagonal = forAll gen_perms (\perm -> isDerangement (perm !! 0) (perm !! 1) --> sub_isDerangement (perm !! 0) (perm !! 1))

manual_test_cases :: [[[Int]]]
manual_test_cases = [
    [[1,2,3], [2,3,1]],
    [[1,2,3], [1,2,3]],
    [[1], [1]],
    [[16,21,3,1], [21,16,1]],
    [[1,1,2,2], [2,2,1,1]]]

gen_manual_cases :: Gen [[Int]]
gen_manual_cases = elements manual_test_cases

--- Manual testsests for isDerangement
man_prop_equal_length :: Property
man_prop_equal_length = forAll gen_manual_cases (\perm -> isDerangement (perm !! 0) (perm !! 1) --> equal_length (perm !! 0) (perm !! 1))

man_prop_same_count :: Property
man_prop_same_count = forAll gen_manual_cases (\perm -> isDerangement (perm !! 0) (perm !! 1) --> all (\x -> same_count (perm !! 0) (perm !! 1) x) (perm !! 0))

man_prop_permutation :: Property
man_prop_permutation = forAll gen_manual_cases (\perm -> isDerangement (perm !! 0) (perm !! 1) --> isPermutation (perm !! 0) (perm !! 1))

man_prop_diagonal :: Property
man_prop_diagonal = forAll gen_manual_cases (\perm -> isDerangement (perm !! 0) (perm !! 1) --> sub_isDerangement (perm !! 0) (perm !! 1))


main = do
    quickCheck prop_equal_length
    quickCheck prop_same_count
    quickCheck prop_permutation
    quickCheck prop_diagonal
    quickCheck man_prop_equal_length
    quickCheck man_prop_same_count
    quickCheck man_prop_permutation
    quickCheck man_prop_diagonal
    print (isDerangement [1,2,3] [2,3,1])
    print (isDerangement [15,14,13] [14,15,13])
    print (isDerangement [15,14,13] [14,15,12])
    print (isDerangement [1,2,2] [2,2,1])
    print (all_permutations [0,1,2])
    print (deran 2)

{- Time taken: 60min
1. I defined isDerangement using two separate properties. One being that is is a permutation (from Exercise4), 
and the other is defined in sub_isDerangement to check recursively whether the diagonal elements have all changed.
2. I generate the derangements by generating a list of all permutations, and filtering out those that do not satisfy the isDerangement property.
To generate all permutations I made a helper function which generates all options for picking a single element and returns it as (x from list, rest of list without x).
This is then used to recurisvely define all permutations, which always picks one element, and iterates all options, and combines this will all permutations of the rest.
3. I created propterties for testing equal length, testing the same count of all elements, testing that they are permutations of each other, and testing the diagonal.
The manual values were chosen to test for edge cases where list were identical, had different sizes, and where multiple elements were the same.
4. Permutation and diagonal are incompariable and at the same level, below both is equal length, as both imply the equal length property. 
Below the permutation at the same level as the equal length property is the same count property.
5. I automated the test process with quickCheck
-}