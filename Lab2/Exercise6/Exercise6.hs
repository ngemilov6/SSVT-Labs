module Exercise6 where

import Data.List (nub, sort)
import Test.QuickCheck
-- import Exercise5 (trClos)
-- import Exercise3 (symClos)
--- FIND WAY TO IMPORT trClos and symClos FROM OTHER EXERCISES

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

symClos :: Ord a => Rel a -> Rel a
symClos rel = sort (nub ([(y,x) | (x,y) <- rel ] ++ rel))

trClos :: Ord a => Rel a -> Rel a
trClos rel = sort (nub ((rel @@ rel) ++ rel))

prop_symmetric :: Ord a => Rel a -> Bool
prop_symmetric rel = all (\(x,y) -> elem (y,x) rel) rel

prop_transitive :: Ord a => Rel a -> Bool
prop_transitive rel = all (\(x,y) -> all (\(a,b) -> if a == y then elem (x,b) rel else True) rel) rel

main :: IO()
main = do
    print (trClos [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)])
    print (prop_transitive (trClos [(1,2),(1,3),(1,4),(2,3),(2,4),(3,4)]))
    print (prop_symmetric (symClos [(1,2), (1,3)]))
    quickCheck prop_symmetric
    