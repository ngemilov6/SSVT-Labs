module Exercise2 where

import MultiplicationTable
import Mutation
import Test.QuickCheck

-- Counts how many generated mutants survive all supplied properties.
-- A mutant survives if it is different from the original output and every property
-- passes for that mutant at the same input value.
countSurvivors :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO Integer
countSurvivors mutantCount properties functionUnderTest =
	fromIntegral . length . filter id <$>
		sequenceA (replicate (fromIntegral (max 0 mutantCount)) generateAndCheckMutant)
	where
		-- Generate one random input, mutate the original output, and
		-- see whether the mutant survives all properties.
		generateAndCheckMutant =
			generate (arbitrary :: Gen Integer) >>= \input ->
				let original = functionUnderTest input
				in generateDifferentMutant original >>= \mutant ->
					all isSuccess <$>
						traverse
							(\propertyUnderTest -> quickCheckWithResult (stdArgs { maxSuccess = 1, chatty = False }) (propertyUnderTest mutant input))
							properties

		-- Mutate until the mutant differs from the original
		generateDifferentMutant original =
			generate (elements mutators) >>= \mutator ->
				generate (mutator original) >>= \mutant ->
					if mutant == original
						then generateDifferentMutant original
						else pure mutant



multiplicationTableProperties :: [([Integer] -> Integer -> Property)]
multiplicationTableProperties =
	map (\propertyUnderTest output input -> property (propertyUnderTest output input)) multiplicationTableProps

alwaysTrueProperty :: [Integer] -> Integer -> Property
alwaysTrueProperty _ _ = property True

main :: IO ()
main = do
    -- Check that the survivor counter works as expected with a property that always returns True
    -- Expected output: 10 survivors
	forcedSurvivors <- countSurvivors 10 [alwaysTrueProperty] multiplicationTable
	putStrLn $ "Survivor counter check (expected 10): " ++ show forcedSurvivors

	survivors <- countSurvivors 4000 multiplicationTableProperties multiplicationTable
	putStrLn $ "Surviving mutants: " ++ show survivors
