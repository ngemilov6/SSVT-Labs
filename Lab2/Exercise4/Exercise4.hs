module Exercise4 where

import SetOrd
import System.Random
import Test.QuickCheck

type Rel a = [(a,a)]

listTuplestoList :: [(a,b)] -> [a]
listTuplestoList [] = []
listTuplestoList ((x,_):xs) = x : listTuplestoList xs


isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial xs rel = all (\x -> elem x (listTuplestoList rel)) xs
