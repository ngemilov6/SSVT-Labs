module Exercise5 where

import Prelude
import Data.List (permutations)
import Test.QuickCheck
import Text.Printf (errorBadArgument)


--- Properties of isPermutation: Equal Length and same elements
equal_length :: Eq a => [a] -> [a] -> Bool
equal_length l1 l2 = length l1 == length l2

same_elements :: Eq a => [a] -> [a] -> Bool
same_elements l1 l2 = all (`elem` l2) l1

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = equal_length xs ys && same_elements xs ys

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && and (zipWith (/=) xs ys)

deran :: Int -> [[Int]]
deran n | n < 0 = error "Bad argument"
        | n == 0    = []
        | otherwise = let list = [0..(n-1)] 
                  in filter (isDerangement list) (permutations list)


prop_deran_valid :: Property
prop_deran_valid = forAll (genSmallNat 1 7) $ \n ->
    let base = [0..(n-1)]
    in all (isDerangement base) (deran n)

prop_derangement_symmetric :: Property
prop_derangement_symmetric =
  forAll gen_perms $ \perm ->
    let xs = perm !! 0
        ys = perm !! 1
    in isDerangement xs ys == isDerangement ys xs

prop_derangement_symmetric_manual :: Property
prop_derangement_symmetric_manual =
  forAll gen_manual_cases $ \(xs, ys) ->
    isDerangement xs ys == isDerangement ys xs

prop_derangement_no_fixed_points :: Property
prop_derangement_no_fixed_points =
    forAll gen_perms $ \perm ->
    let xs = perm !! 0
        ys = perm !! 1
    in isDerangement xs ys ==> and (zipWith (/=) xs ys)

prop_derangement_no_fixed_points_manual :: Property
prop_derangement_no_fixed_points_manual =
  forAll gen_manual_cases $ \(xs, ys) ->
    isDerangement xs ys ==> and (zipWith (/=) xs ys)

manual_test_cases :: [([Int], [Int])]
manual_test_cases = [
    ([0,1,2,3], [2,3,1,0]),
    ([0,1,2,3], [0,1,2,3]),
    ([1], [1]),
    ([16,21,3,1], [21,16,1]),
    ([0,0,1,1,2,2], [2,2,1,1,0,0])]

gen_manual_cases :: Gen ([Int], [Int])
gen_manual_cases = elements manual_test_cases


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
genList 0 = pure []
genList n = do
    perm <- makePerm [0..(n-1)]
    pure perm


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
    putStrLn "1. Testing deran generator validity..."
    quickCheck prop_deran_valid
    
    putStrLn "2. Testing symmetry..."
    quickCheck prop_derangement_symmetric
    quickCheck prop_derangement_symmetric_manual

    
    putStrLn "3. Testing no fixed points implication..."
    quickCheck prop_derangement_no_fixed_points
    quickCheck prop_derangement_no_fixed_points_manual