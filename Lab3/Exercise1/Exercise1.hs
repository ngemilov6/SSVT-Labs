module Exercise1 where

import Data
import MultiplicationTable
import Mutation

-- Not covered:
-- Changing first (or any) element to a different value
-- Changing step size (difference between consecutive elements)
-- Empty list
-- Invalid input (e.g., negative numbers, zero, etc.)
-- Invalid type (e.g., non-integer, etc.)

changeElement :: [Integer] -> Gen [Integer]
changeElement [] = return [0]
changeElement xs = do
  index <- choose (0, length xs - 1)
  candidate <- arbitrary :: Gen Integer
  -- check to guarantee that the new value is different from the old value, otherwise we will get a false positive
  let oldValue = xs !! index
      newValue = if candidate == oldValue then oldValue + 1 else candidate
  let newList = take index xs ++ [newValue] ++ drop (index + 1) xs
  return newList

emptyList :: [Integer] -> Gen [Integer]
emptyList _ = return []

invalidInput :: [Integer] -> Gen [Integer]
invalidInput _ = return []

invalidType :: [Integer] -> Gen [Integer]
invalidType _ = return []