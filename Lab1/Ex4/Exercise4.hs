import Prelude
import Data.List (nub)
import Test.QuickCheck ( quickCheck )

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` ys) xs

prop_reflexive :: [Int] -> Bool
prop_reflexive xs = 
    let uniqueXs = nub xs 
    in isPermutation uniqueXs uniqueXs

prop_reverse :: [Int] -> Bool
prop_reverse xs = 
    let uniqueXs = nub xs 
    in isPermutation uniqueXs (reverse uniqueXs)

prop_symmetric :: [Int] -> [Int] -> Bool
prop_symmetric xs ys = 
    let uniqueXs = nub xs
        uniqueYs = nub ys
    in isPermutation uniqueXs uniqueYs == isPermutation uniqueYs uniqueXs

main :: IO ()
main = do
    putStrLn "Running QuickCheck Properties for isPermutation..."
    
    putStrLn "\n1. Reflexivity:"
    quickCheck prop_reflexive
    
    putStrLn "2. Reverse:"
    quickCheck prop_reverse
    
    putStrLn "3. Symmetry:"
    quickCheck prop_symmetric