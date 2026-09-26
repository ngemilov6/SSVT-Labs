module Exercise5 (trClos, main) where

import Data.List (nub, sort)
import Test.QuickCheck ( property, resize, quickCheck, forAll, arbitrary, Property, Gen )

type Rel a = [(a,a)]

infixr 5 @@
(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [ (x,z) | (x,y) <- r, (w,z) <- s, y == w ]

--- From lecture
fix :: (a -> a) -> a
fix f = f (fix f)

--- trClos implemented without fix, with a manual recursion
trClos :: Ord a => Rel a -> Rel a
trClos rel =
    let r  = sort (nub rel)
        r' = sort (nub (r ++ r @@ r))
    in if r' == r then r else trClos r'

--- trClos implemented using fix
trClosFix :: Ord a => Rel a -> Rel a
trClosFix rel = fix (\f r -> 
    let r' = sort (nub (r ++ r @@ r))
    in if r' == r then r else f r') (sort (nub rel))

--- property to test that both functions deliver the same result
prop_reference :: Property
prop_reference =
  forAll (arbitrary :: Gen (Rel Integer)) $ \rel ->
    trClos rel == trClosFix rel

main :: IO()
main = do
    print (trClos [(1,2),(2,3),(3,4)])
    print (trClosFix [(1,2),(2,3),(3,4)])
    quickCheck prop_reference
