module Exercise3 where
<<<<<<< HEAD

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List
=======
    
import Data.List (nub, sort)
>>>>>>> ed68974973bb76b622917150b9706beb21100f3b

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
<<<<<<< HEAD
symClos xs = sort (xs ++ [(y,x) | (x,y) <- xs])
=======
symClos [] = []
symClos rel = sort $ nub ([(y,x)| (x,y) <-rel] ++ rel)
>>>>>>> ed68974973bb76b622917150b9706beb21100f3b
