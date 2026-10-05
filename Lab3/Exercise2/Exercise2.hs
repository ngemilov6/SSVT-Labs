module Exercise2 where

import Test.QuickCheck
import Mutation (mutate')
import MultiplicationTable (multiplicationTableProps, multiplicationTable)
import Control.Monad (replicateM)

-- Modified signature to align with `mutate'` type parameters and actual properties.
countSurvivors :: Integer -> [[Integer] -> Integer -> Bool] -> (Integer -> [Integer]) -> ([Integer] -> Gen [Integer]) -> IO Integer
countSurvivors numMutants props fut mutator = do
    -- Generate numMutants cases. We use a fixed input or could randomize it.
    let input = 5
    results <- generate $ replicateM (fromIntegral numMutants) (mutate' mutator props fut input)
    
    -- A mutant survives if it produced an output (not empty) and passed all properties (all True)
    let survivors = filter (\res -> not (null res) && and res) results
    return $ fromIntegral (length survivors)