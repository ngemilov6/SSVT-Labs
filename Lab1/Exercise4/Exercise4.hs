module Exercise4 where

import Prelude
import Data.List (nub)
import Test.QuickCheck ( quickCheck, forAll, chooseInt, Property, Gen )

--- Properties of isPermutation: Equal Length and same elements
equal_length :: [a] -> [a] -> Bool
equal_length l1 l2 = length l1 == length l2

same_elements :: [a] -> [a] -> Bool
same_elements l1 l2 = all (`elem` l2) l1

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = equal_length xs ys && same_elements xs ys

--- Properties
prop_reflexive :: Property
prop_reflexive = forAll gen_perm (\perm -> isPermutation perm perm)

prop_reverse :: Property
prop_reverse = forAll gen_perm (\perm -> isPermutation perm (reverse perm))

prop_symmetric :: Property
prop_symmetric = forAll gen_perms (\perm -> isPermutation (perm !! 0) (perm !! 1)
    == isPermutation (perm !! 1) (perm !! 0))

falsified :: [Int] -> [Int]
falsified (x:xs) = ((x+1):xs)

prop_faulty_permutation :: Property
prop_faulty_permutation = forAll gen_perm (\perm -> not (isPermutation perm (falsified perm)))


--- Generation of permutations
--- Generate a random number within bounds, adjusted from previous exercises and lecture material
genSmallNat :: Int -> Int -> Gen Int
genSmallNat a b = chooseInt (a, b)

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

--- Generates a list of n random input numbers between 1 and and supplied maximum value
genList :: Int -> Gen [Int]
genList 0 _ = pure []
genList n = do
    ns <- makePerm [1..(n+10)]
    pure (take n ns)


--- Generator for permutations, based on a random length within bounds and a random maximum value within bounds.
--- An initial permutation is generated, and from that, a new permutation.
gen_perms :: Gen [[Int]]
gen_perms = do
    length_perm <- genSmallNat 3 10
    init_perm <- genList length_perm
    perm <- makePerm init_perm
    pure [init_perm, perm]

gen_perm :: Gen [Int]
gen_perm = do
    length_perm <- genSmallNat 3 10
    init_perm <- genList length_perm
    pure init_perm

main :: IO ()
main = do
    putStrLn "Running QuickCheck Properties for isPermutation..."
    
    putStrLn "\n1. Reflexivity:"
    quickCheck prop_reflexive
    
    putStrLn "2. Reverse:"
    quickCheck prop_reverse
    
    putStrLn "3. Symmetry:"
    quickCheck prop_symmetric

    putStrLn "4. Faulty Permutation"
    quickCheck prop_faulty_permutation

    -- print (isPermutation [1,2,4] [4,2,4])
    -- print (isPermutation [1,2,4,5] [1,2,4])
    -- print (isPermutation [1,2,3,4] [1,2,3,4])
    -- print (isPermutation [1,2,3,4] [1,2,4,3])
    -- print (isPermutation "abc" "acb")
