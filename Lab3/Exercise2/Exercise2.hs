module Exercise2 where

import MultiplicationTable
import Mutation
import Test.QuickCheck

-- Counts how many generated mutants survive all supplied properties.
-- A mutant survives if it is different from the original output and every property
-- passes for that mutant at the same input value.
countSurvivors :: Integer -> [[Integer] -> Gen [Integer]] -> [([Integer] -> Integer -> Bool)] -> (Integer -> [Integer]) -> IO Integer
countSurvivors mutantCount mutators properties functionUnderTest =
	fromIntegral . length . filter id <$>
		sequenceA (replicate (fromIntegral (max 0 mutantCount)) generateAndCheckMutant)
	where
		generateAndCheckMutant =
			generate (arbitrary :: Gen Integer) >>= \input ->
				let original = functionUnderTest input
				in generateDifferentMutant original >>= \mutant ->
					pure $ all (\propertyUnderTest -> propertyUnderTest mutant input) properties

		generateDifferentMutant original =
			generate (elements mutators) >>= \mutator ->
				generate (mutator original) >>= \mutant ->
					if mutant == original
						then generateDifferentMutant original
						else pure mutant

alwaysTrueProperty :: [Integer] -> Integer -> Bool
alwaysTrueProperty _ _ = True


main :: IO ()
main = do
    -- Check that the survivor counter works as expected with a property that always returns True
    -- Expected output: 10 survivors
	forcedSurvivors <- countSurvivors 10 mutators [alwaysTrueProperty] multiplicationTable
	putStrLn $ "Survivor counter check (expected 10): " ++ show forcedSurvivors

	survivors <- countSurvivors 4000 mutators multiplicationTableProps multiplicationTable
	putStrLn $ "Surviving mutants: " ++ show survivors

