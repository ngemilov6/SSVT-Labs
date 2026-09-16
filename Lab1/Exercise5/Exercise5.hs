import Prelude
import Data.List (permutations)
import Test.QuickCheck

isPermutation :: Eq a => [a] -> [a] -> Bool
isPermutation xs ys = length xs == length ys && all (`elem` ys) xs

isDerangement :: Eq a => [a] -> [a] -> Bool
isDerangement xs ys = isPermutation xs ys && and (zipWith (/=) xs ys)

deran :: Int -> [[Int]]
deran n | n <= 0    = []
        | otherwise = let list = [0..n-1] 
                  in filter (isDerangement list) (permutations list)

genSmallNat :: Gen Int
genSmallNat = chooseInt (1, 7)

prop_deran_valid :: Property
prop_deran_valid = forAll genSmallNat $ \n ->
    let base = [0..n-1]
    in all (isDerangement base) (deran n)

prop_derangement_symmetric :: [Int] -> [Int] -> Bool
prop_derangement_symmetric xs ys = 
    isDerangement xs ys == isDerangement ys xs

prop_derangement_no_fixed_points :: [Int] -> [Int] -> Property
prop_derangement_no_fixed_points xs ys =
    isDerangement xs ys ==> and (zipWith (/=) xs ys)

main :: IO ()
main = do
    putStrLn "1. Testing deran generator validity..."
    quickCheck prop_deran_valid
    
    putStrLn "2. Testing symmetry..."
    quickCheck prop_derangement_symmetric
    
    putStrLn "3. Testing no fixed points implication..."
    quickCheck prop_derangement_no_fixed_points