module Exercise1 where
import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace


-- == Mutators ==
-- Changes elements
changeElements :: [Integer] -> Gen [Integer]
changeElements [] = pure []
changeElements xs = do
    num <- arbitrary :: Gen Integer
    index <- choose (0, (length xs) - 1)
    return $ take index xs ++ [num] ++ drop (index + 1) xs

add1Elements :: [Integer] -> Gen [Integer]
add1elements xs = do
    return $ [x+1 | x<-xs]

permuteElements :: [Integer] -> Gen [Integer]
permuteElements xs = shuffle xs

