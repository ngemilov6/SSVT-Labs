module Exercise2 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1
-- import Lecture2
-- import Lecture3

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

main :: IO ()
main = do 
    quickCheck prop_powerSetCardinality
    quickCheck prop_powerSetNoDuplicates