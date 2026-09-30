module Exercise6 where

import Data.List (nub, sort)
import Test.QuickCheck
import Exercise3 (symClos)
import Exercise5 (trClos, (@@))

type Rel a = [(a,a)]

isSymmetric :: Eq a => Rel a -> Bool
isSymmetric rel = all (\(x,y) -> (y,x) `elem` rel) rel

isTransitive :: Eq a => Rel a -> Bool
isTransitive rel = all (`elem` rel) (rel @@ rel)

isSubset :: Eq a => Rel a -> Rel a -> Bool
isSubset r1 r2 = all (`elem` r2) r1


-- property to ensure the resulting closure is perfectly symmetric.
prop_symClos_isSymmetric :: Rel Int -> Bool
prop_symClos_isSymmetric rel = isSymmetric (symClos rel)

-- property to verify the original relation is a subset of its symmetric closure.
prop_symClos_containsOriginal :: Rel Int -> Bool
prop_symClos_containsOriginal rel = rel `isSubset` symClos rel

-- property to check that applying symmetric closure twice yields the same result.
prop_symClos_idempotent :: Rel Int -> Bool
prop_symClos_idempotent rel = symClos (symClos rel) == symClos rel


-- property to ensure all (x,y) and (y,z) paths have a direct (x,z) edge in the closure.
prop_trClos_isTransitive :: Rel Int -> Bool
prop_trClos_isTransitive rel = isTransitive (trClos rel)

-- property to verify the original relation is a subset of its transitive closure.
prop_trClos_containsOriginal :: Rel Int -> Bool
prop_trClos_containsOriginal rel = rel `isSubset` trClos rel

-- property to check that applying transitive closure twice yields the same result.
prop_trClos_idempotent :: Rel Int -> Bool
prop_trClos_idempotent rel = trClos (trClos rel) == trClos rel


main :: IO ()
main = do
    putStrLn "--- Testing symClos ---"
    putStrLn "Testing if output is symmetric:"
    quickCheck prop_symClos_isSymmetric
    putStrLn "Testing if output contains original relation:"
    quickCheck prop_symClos_containsOriginal
    putStrLn "Testing idempotence:"
    quickCheck prop_symClos_idempotent

    putStrLn "\n--- Testing trClos ---"
    putStrLn "Testing if output is transitive:"
    quickCheck prop_trClos_isTransitive
    putStrLn "Testing if output contains original relation:"
    quickCheck prop_trClos_containsOriginal
    putStrLn "Testing idempotence:"
    quickCheck prop_trClos_idempotent