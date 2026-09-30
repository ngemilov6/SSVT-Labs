import Data.List (foldl')

sumOddSquares :: Integer -> Integer
sumOddSquares n = foldl' (\acc x -> acc + x*x) 0 [1, 3 .. n]

main :: IO ()
main = do
    let n = 984000
    putStrLn "Among the first 984 thousand square numbers, what is the sum of all the odd squares?"
    print (sumOddSquares n)