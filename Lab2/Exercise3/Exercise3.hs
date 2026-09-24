module Exercise3 (symClos) where

import Data.List (nub, sort)


type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos rel = sort (nub ([(y,x) | (x,y) <- rel ] ++ rel))


main :: IO()
main = do
    print (symClos [(1,2), (1,3)])