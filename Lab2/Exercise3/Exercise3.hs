module Exercise3 where

import Test.QuickCheck
import SetOrd

-- run with:
-- runghc --ghc-arg=-i.. Exercise3.hs +RTS -M512M -RTS

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos r = let Set pairs = list2set (r ++ [(y,x) | (x,y) <- r])
            in pairs

main ::  IO ()
main = do
   -- Tests, not exhaustive; expected output is True for all tests
      let r1 = [(1,2),(2,3)]
      let r2 = [(1,2),(2,1),(2,3),(3,2)]
      let r3 = [(1,2),(2,3),(3,1)]
      let r4 = [(1,2),(2,1),(2,3),(3,2),(3,1),(1,3)]
      
      print (symClos r1 == r2)
      print (list2set (symClos r3) == list2set r4)