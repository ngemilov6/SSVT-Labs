module Exercise5 (trClos) where

import Data.List (nub, sort)

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

trClos :: Ord a => Rel a -> Rel a
trClos rel = let trans = rel @@ rel in 
    if trans == [] then rel else sort (nub (rel ++ trans ++ rel @@ (trClos trans)))

main :: IO()
main = do
    print (trClos [(1,2),(2,3),(3,4)])
