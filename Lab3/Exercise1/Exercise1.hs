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

-- Changes type
changeType :: [Integer] -> Gen [String]
changeType xs = pure (map show xs)

permuteElements :: [Integer] -> Gen [Integer]
permuteElements xs = shuffle xs

