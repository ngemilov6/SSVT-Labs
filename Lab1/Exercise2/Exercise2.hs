module Exercise2 where

import Data.List
import Test.QuickCheck

-- recursively finds the power set of a list
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = let p = powerSet xs in p ++ map (x:) p

-- property to check cardinality property of the power set
prop_powerSetCardinality :: Property
prop_powerSetCardinality =
    -- resize to avoid large lists
  forAll (resize 10 (listOf (choose (0 :: Int, 10)))) ( \xs -> length (powerSet xs) == 2 ^ length xs )

-- property to check that the power set contains no duplicates
prop_powerSetNoDuplicates :: Property
prop_powerSetNoDuplicates =
    -- sublist to avoid duplicates in the input list
  forAll (sublistOf [0 :: Int .. 9]) ( \xs ->
    let ps = powerSet xs
    in length ps == length (nub ps) )

-- property to check cardinality property of the power set for list of the form [1..n]
prop_powerSetCardinality_1_to_n :: Property
prop_powerSetCardinality_1_to_n =
  forAll (choose (1, 15 :: Int)) $ \n ->
    let xs = [1..n]
    in length (powerSet xs) == 2 ^ length xs

-- property to check if output elements are actually sublists
prop_powerSetSoundness :: Property
prop_powerSetSoundness =
  forAll (resize 10 (listOf (choose (0 :: Int, 10)))) $ \xs ->
    all (`isSubsequenceOf` xs) (powerSet xs)

main :: IO ()
main = do 
    quickCheck prop_powerSetCardinality
    quickCheck prop_powerSetNoDuplicates
    quickCheck prop_powerSetCardinality_1_to_n
    quickCheck prop_powerSetSoundness