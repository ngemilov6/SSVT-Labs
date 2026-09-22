module Euler49 where

import Test.QuickCheck
import Data.List (sort, tails)

--- From Lectures
prime :: Int -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Int]
primes = 2 : filter prime [3..]

--- Filters by choosing two increasing numbers and checks if the thid number in the arithmetic sequence is still an element of the prime list
arithmeticTriples :: [Int] -> [(Int, Int, Int)]
arithmeticTriples ps = 
    [ (a, b, c)
    | a : rest <- tails ps,
    b <- rest,
    let c = 2 * b - a,
    c `elem` rest,
    arePermutations a b c]

--- Checks whether numbers are permutations of each other
arePermutations :: Int -> Int -> Int -> Bool
arePermutations a b c = sort (show a) == sort (show b) && sort (show a) == sort (show c)

--- Generates list of primes wit ha certain number of digits
primesOfLength :: Int -> [Int]
primesOfLength n = filter prime [(10^(n-1))..(10^n-1)]

--- Concatenate to print the number
concatNumber :: (Int,Int,Int) -> Integer
concatNumber (a,b,c) = read (show a ++ show b ++ show c)

prop_isArithmetic :: Int -> Int -> Int -> Bool
prop_isArithmetic a b c = 2*b - a == c

splitNum :: Integer -> [Int]
splitNum n =
    [ read (take 4 digits),
    read (take 4 (drop 4 digits)),
    read (take 4 (drop 8 digits))]
    where
        digits = show n

euler49 :: Integer
euler49 =
    let result = arithmeticTriples (primesOfLength 4)
    in concatNumber (result !! 1) --- As first is the example given


main :: IO ()
main = do
    let result = euler49
    putStrLn "\nEuler 49 Output (12-digit concatenated number):"
    print result

    putStrLn "Confirming whether solution actually has the properties required:"
    let ns@(a : b : c : _) = splitNum result
    putStrLn "(1) all 3 prime:"
    print (all (\n -> prime n) ns)
    putStrLn "(2) form an arithmetic sequence"
    print (prop_isArithmetic a b c)
    putStrLn "(3) permutations of each other"
    print (arePermutations a b c)
    