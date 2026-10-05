module Exercise1 where

import Test.QuickCheck
import Mutation (addElements, removeElements, anyList)

-- Mutator 1: Value modification - changes exactly one random element in the list.
-- This includes a great safeguard to ensure the new value is strictly different 
-- from the old value, preventing accidental "equivalent mutants" that inflate survival rates.
changeElement :: [Integer] -> Gen [Integer]
changeElement [] = return [0] -- Edge case handling: mutating an empty list yields [0]
changeElement xs = do
  index <- choose (0, length xs - 1)
  candidate <- arbitrary :: Gen Integer
  
  -- Check to guarantee that the new value is different from the old value
  let oldValue = xs !! index
      newValue = if candidate == oldValue then oldValue + 1 else candidate
      
  -- Reconstruct the list with the newly mutated element
  let newList = take index xs ++ [newValue] ++ drop (index + 1) xs
  return newList

-- Mutator 2: Order modification - shuffles the list elements.
-- Strong against properties that check for specific ordering (like prop_linear).
shuffleElements :: [Integer] -> Gen [Integer]
shuffleElements = shuffle

-- Mutator 3: Value modification - increments all elements by 1.
-- Strong against mathematical properties (like prop_moduloIsZero).
incrementElements :: [Integer] -> Gen [Integer]
incrementElements xs = return $ map (+1) xs

-- Mutator 4: Structural modification - completely empties the list.
-- Consolidated from 'invalidInput' and 'invalidType' as Haskell's strict 
-- typing forces all these structural violations to simply be empty integer lists.
emptyList :: [Integer] -> Gen [Integer]
emptyList _ = return []

-- Active Mutators List
-- Fixed inconsistency: Updated to use the correct 'changeElement' name and added 'emptyList'.
myMutators :: [[Integer] -> Gen [Integer]]
myMutators = [changeElement, shuffleElements, incrementElements, emptyList]