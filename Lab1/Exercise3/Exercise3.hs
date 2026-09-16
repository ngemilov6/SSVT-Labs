import Prelude

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

stronger :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
stronger xs p q = all (\x -> p x --> q x) xs

weaker :: [a] -> (a -> Bool) -> (a -> Bool) -> Bool
weaker xs p q = stronger xs q p

prop1, prop2, prop3, prop4 :: Int -> Bool
prop1 x = even x && x > 3
prop2 x = even x
prop3 x = even x || x > 3
prop4 x = (even x && x > 3) || even x

domain :: [Int]
domain = [-10 .. 10]

main :: IO ()
main = do
    putStrLn "Comparing property strengths...\n"
    
    putStrLn $ "Is prop1 stronger than prop2? " ++ show (stronger domain prop1 prop2)
    putStrLn $ "Is prop2 stronger than prop3? " ++ show (stronger domain prop2 prop3)
    putStrLn $ "Is prop4 identical in strength to prop2? " ++ 
               show (stronger domain prop2 prop4 && stronger domain prop4 prop2)
    
    putStrLn "\n--- Descending Strength List ---"
    putStrLn "1. prop1 (Strongest)"
    putStrLn "2. prop2 & prop4 (Equivalent level)"
    putStrLn "3. prop3 (Weakest)"