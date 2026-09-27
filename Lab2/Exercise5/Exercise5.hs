module Exercise5 where

import Data.List
import Lecture4

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y==w]

trClos :: Ord a => Rel a -> Rel a
trClos = fp (\r -> sort $ nub ((r @@ r) ++ r))