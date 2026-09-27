module Exercise5 where

import SetOrd
import System.Random
import Test.QuickCheck
import Data.List

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s =
    nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rell = until (\r -> all (`elem` r) (r @@ r)) (\r -> sort(nub (r ++ (r @@ r)))) rell