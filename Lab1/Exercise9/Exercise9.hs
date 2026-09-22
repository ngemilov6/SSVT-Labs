{-# LANGUAGE NoExplicitForAll #-}

module Exercise9 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1
-- import Lecture2
-- import Lecture3


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- forall :: [a] -> (a -> Bool) -> Bool
-- forall = flip all
---

--- From Lecture 1
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]
--- ^From Lecture 1

--- Infinite list of prime products as the products of slices from the start of primes
prime_product :: [Integer]
prime_product = [product (take n primes) | n<-[1..]]

--- Infinite list of counter examples for the conjecture
counterexamples :: [([Integer], Integer)]
counterexamples = [(l, pr + 1)|n<-[1..], let l = (take n primes), let pr = product (l), not (prime (pr + 1))]

--- ^From Lecture 3
---
---
main :: IO()
main = do 
    print (take 10 prime_product)
    print (take 5 counterexamples)
    


{- Time taken: 10min
I used the definitions for prime and primes from lecture 1.
From there, I created an infinite list of prime products as the product of slices of length in [1..], 
which I do not end up using in the final solution, but is where the final solution is motivated.
I can then find counter examples by going through the integers [1..], 
taking the according slice of primes (l), taking the product of that list (pr), and then checking 
whether the following number (pr+1) is not a prime. If so, I add the list of primes 
and the contradicting number to the output.
-}