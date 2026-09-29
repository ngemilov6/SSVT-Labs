module Exercise5 where

import Data.List
import Lecture4

type Rel a = [(a,a)]

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y==w]

trClos :: Ord a => Rel a -> Rel a
trClos = fp (\r -> sort $ nub ((r @@ r) ++ r))

trClos2 :: Ord a => Rel a -> Rel a
trClos2 r = until (\r -> all (`elem` r) (r @@ r)) (\r -> sort(nub (r ++ (r @@ r))))