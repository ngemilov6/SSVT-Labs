module Exercise6 where

import Test.QuickCheck
import Exercise2 (countSurvivors)
import Exercise4 (propertyStrength)
import MultiplicationTable (multiplicationTableProps, multiplicationTable)
import Mutation (addElements, removeElements, anyList)

visualizeResults :: Integer -> IO ()
visualizeResults numMutants = do
    putStrLn "=== Mutation Testing Analysis ==="
    
    -- Evaluate against one of the provided mutators
    survivors <- countSurvivors numMutants multiplicationTableProps multiplicationTable addElements
    
    let strength = propertyStrength numMutants survivors
    
    putStrLn $ "Mutator Applied: addElements"
    putStrLn $ "Total Generated Mutants: " ++ show numMutants
    putStrLn $ "Properties Evaluated: " ++ show (length multiplicationTableProps)
    putStrLn $ "Surviving Mutants: " ++ show survivors
    putStrLn $ "Property Suite Strength: " ++ show strength ++ "%"
    putStrLn "================================="

-- Required QuickCheck tests for Exercise 6 verifying calculation bounds
prop_strengthValidBounds :: NonNegative Integer -> NonNegative Integer -> Property
prop_strengthValidBounds (NonNegative total) (NonNegative survivors) =
    total > 0 ==> 
        let validSurv = min total survivors
            score = propertyStrength total validSurv
        in score >= 0.0 && score <= 100.0

runVisualizationTests :: IO ()
runVisualizationTests = do
    putStrLn "Executing QuickCheck properties for reporting logic..."
    quickCheck prop_strengthValidBounds