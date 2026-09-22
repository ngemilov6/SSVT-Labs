{-# LANGUAGE NoExplicitForAll #-}

module Exercise9 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture1 (prime, primes)


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

--- Infinite list of prime products as the products of slices from the start of primes
prime_product :: [Integer]
prime_product = [product (take n primes) | n<-[1..]]

--- Infinite list of counter examples for the conjecture
counterexamples :: [([Integer], Integer)]
counterexamples = [(l, pr + 1)|n<-[1..], let l = (take n primes), let pr = product (l), not (prime (pr + 1))]

main :: IO()
main = do 
    print (take 10 prime_product)
    print (take 5 counterexamples)
    