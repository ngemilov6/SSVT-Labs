import Data.List

type Rel a = [(a,a)]

symClos :: Ord a => Rel a -> Rel a
symClos [] = []
symClos rel = sort ( nub ([(y,x)| (x,y) <-rel] ++ rel))

infixr 5 @@

(@@) :: Eq a => Rel a -> Rel a -> Rel a
r @@ s = nub [(x,z) | (x,y) <- r, (w,z) <- s, y==w]

trClos :: Ord a => Rel a -> Rel a
trClos rel | rel == r' = rel
           | otherwise = trClos r'
        where r' = sort $ nub ((rel @@ rel) ++ rel)
