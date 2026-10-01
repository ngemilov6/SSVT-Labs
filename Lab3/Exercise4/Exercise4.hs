module Exercise4 where

import MultiplicationTable
import Mutation
import Exercise2

propertyStrength ::
    Integer 
    -> [([Integer] -> Integer -> Bool)] 
    -> (Integer -> [Integer]) 
    -> IO Float
propertyStrength n properties functionUnderTest
    | n <= 0 = error "propertyStrength: number of mutants must be positive"
    | otherwise = do
        survivors <- countSurvivors n mutators properties functionUnderTest
        pure $ 1 - fromIntegral survivors / fromIntegral n

main :: IO ()
main = do
    let properties = [prop_firstElementIsInput]
    strength <- propertyStrength 4000 properties multiplicationTable
    putStrLn $ "Strength of the property set: " ++ show strength


