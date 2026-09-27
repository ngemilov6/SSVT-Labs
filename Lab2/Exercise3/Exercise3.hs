module Exercise3 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos xs = sort (xs ++ [(y,x) | (x,y) <- xs])