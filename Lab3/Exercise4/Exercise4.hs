module Exercise4 where

import MultiplicationTable
import Mutation
import Test.QuickCheck
import Exercise2 hiding (main)

-- run with: runghc --ghc-arg=-i../:../Exercise2 Exercise4.hs +RTS -M512M -RTS

-- Strength is the percentage of mutants killed by a property set
-- We reuse the same mutation pattern as Exercise 2: generate a changed mutant,
-- then count it as killed if any property fails for that mutant
strength :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO Double
strength mutantCount properties functionUnderTest = do
  survivors <- countSurvivors mutantCount mutators properties functionUnderTest
  let total = fromIntegral (max 0 mutantCount)
      killed = total - fromIntegral survivors
  if total == 0
    then return 0
    else return $ (killed / total) * 100

-- Property set for testing
weakPropertySet :: [([Integer] -> Integer -> Property)]
weakPropertySet = [alwaysTrueProperty]

-- This property set is intentionally leaving some mutants alive for testing
midStrengthPropertySet :: [([Integer] -> Integer -> Property)]
midStrengthPropertySet =
    [\output input -> property (length output == 10), alwaysTrueProperty]

main :: IO ()
main = do
    -- Test the built-in multiplication-table properties
    exampleStrength <- strength 4000 multiplicationTableProperties multiplicationTable
    weakStrength <- strength 4000 weakPropertySet multiplicationTable
    midStrength <- strength 4000 midStrengthPropertySet multiplicationTable

    -- Expected 100
    putStrLn $ "Strength: " ++ show exampleStrength
    -- Expected 0
    putStrLn $ "Weak property set strength: " ++ show weakStrength
    -- Expected between 0 and 100
    putStrLn $ "Mid property set strength: " ++ show midStrength
