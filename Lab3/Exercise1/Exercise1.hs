module Exercise1 where

import Test.QuickCheck
import Mutation (addElements, removeElements, anyList)

-- Mutator 1: Value modification - changes exactly one random element in the list
changeOneElement :: [Integer] -> Gen [Integer]
changeOneElement [] = return []
changeOneElement xs = do
    idx <- choose (0, length xs - 1)
    val <- arbitrary
    return $ take idx xs ++ [val] ++ drop (idx + 1) xs

-- Mutator 2: Order modification - shuffles the list elements
shuffleElements :: [Integer] -> Gen [Integer]
shuffleElements xs = shuffle xs

-- Mutator 3: Value modification - increments all elements by 1
incrementElements :: [Integer] -> Gen [Integer]
incrementElements xs = return $ map (+1) xs

myMutators :: [[Integer] -> Gen [Integer]]
myMutators = [changeOneElement, shuffleElements, incrementElements]