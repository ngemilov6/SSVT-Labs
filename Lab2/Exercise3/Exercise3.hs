module Exercise3 where
    
import Data.List (nub, sort)

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos rel = sort $ nub ([(y,x)| (x,y) <-rel] ++ rel)
