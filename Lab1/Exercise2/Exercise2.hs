import Prelude
import Test.QuickCheck

powerset :: [a] -> [[a]]
powerset = buildSubsets [[]]
  where
    buildSubsets :: [[a]] -> [a] -> [[a]]
    buildSubsets acc [] = acc
    buildSubsets acc (x:xs) = buildSubsets (acc ++ map (\subset -> subset ++ [x]) acc) xs

genSmallNat :: Gen Integer
genSmallNat = chooseInteger (0, 15)

prop_powerset_size :: Property
prop_powerset_size = 
    forAll genSmallNat (\n -> toInteger (length (powerset [1..n])) == 2 ^ n)

main :: IO ()
main = do
    putStrLn "Testing Powerset Cardinality..."
    quickCheck prop_powerset_size