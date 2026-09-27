module Exercise2 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

setIntersection :: Ord a => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = list2set [x | x <- xs, x `elem` ys]

setUnion :: Ord a => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

setDifference :: Ord a => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = list2set [x | x <- xs, not (x `elem` ys)]