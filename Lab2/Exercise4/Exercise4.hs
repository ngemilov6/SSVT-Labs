module Exercise4 where

import Data.List

type Rel a = [(a,a)]

hasRelation :: Eq a => [a] -> Rel a -> a -> Bool
hasRelation domain rel n = any (\(x,y) -> x == n && elem y domain) rel

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (\n -> hasRelation domain rel n) domain

--- Mention in documentation base cases where domain is empty and/or relation is empty

--- QuickCheck: elements are all in domain, relation size greater or equal to domain

main :: IO()
main = do
    print (isSerial [1,2,3] [(1,4), (2,3)])
    print (isSerial [1,2,3] [(1,3), (2,3), (3,1)])