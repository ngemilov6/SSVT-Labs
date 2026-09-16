module Exercise1 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1
-- import Lecture2
-- import Lecture3

factorial :: Integer -> Integer
factorial 0 = 1 -- return 1 for the base case
factorial n = n * factorial (n - 1) -- return element as recursive factorial

-- check if recursive factorial equals product factorial
prop_factorialEqualsProduct :: Property
prop_factorialEqualsProduct = forAll (choose (1, 100)) 
    (\n -> factorial n == product [1..n]) 

-- check if 0! = 1
prop_factorialZero :: Property
prop_factorialZero = property (factorial 0 == 1) 

-- check if factorial grows slower than a double exponential function
prop_factorialGrowth :: Property
prop_factorialGrowth = forAll (choose (2, 100))
    (\n -> factorial n < n ^ n) 

main :: IO ()
main = do
    quickCheck prop_factorialEqualsProduct
    quickCheck prop_factorialZero
    quickCheck prop_factorialGrowth