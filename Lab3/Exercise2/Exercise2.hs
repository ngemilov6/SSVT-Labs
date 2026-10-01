module Exercise2 where
import Test.QuickCheck
import Data.List
import MultiplicationTable
import Data.Maybe
import Debug.Trace
import Mutation
import Data.List (intercalate, transpose)
import Text.Printf (printf)

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


---
--- vvv START Visual Output Generated with AI
---
-- A cell such as: "3120/4000 (78.0%)"
formatKillResult :: Integer -> Integer -> String
formatKillResult total survivors =
    let killed = total - survivors
        percentage :: Double
        percentage = 100 * fromIntegral killed / fromIntegral total
    in show killed ++ "/" ++ show total ++
       " (" ++ printf "%.1f" percentage ++ "%)"

padRight :: Int -> String -> String
padRight width text =
    text ++ replicate (width - length text) ' '

renderTable :: [[String]] -> String
renderTable rows =
    let columnWidths = map (maximum . map length) (transpose rows)
        renderRow row =
            intercalate " | " (zipWith padRight columnWidths row)
        separator =
            intercalate "-+-" (map (\width -> replicate width '-') columnWidths)
    in unlines $
        case rows of
            []       -> []
            header:xs -> renderRow header : separator : map renderRow xs

propertySets =
    [ ("All properties", multiplicationTableProps)
    , ("P1: ten elements", [prop_tenElements])
    , ("P2: first element", [prop_firstElementIsInput])
    , ("P3: triangle sum", [prop_sumIsTriangleNumberTimesInput])
    , ("P4: linear", [prop_linear])
    , ("P5: modulo zero", [prop_moduloIsZero])
    ]

mutantSets =
    [ ("All mutators", mutators)
    , ("M1: anyList", [anyList])
    , ("M2: removeElements", [removeElements])
    , ("M3: addElements", [addElements])]

--- ^^^ END AI generation

main :: IO ()
main = do
    -- Check that the survivor counter works as expected with a property that always returns True
    -- Expected output: 10 survivors
    forcedSurvivors <- countSurvivors 10 mutators [alwaysTrueProperty] multiplicationTable
    putStrLn $ "Survivor counter check (expected 10): " ++ show forcedSurvivors

    survivors <- countSurvivors 4000 mutators multiplicationTableProps multiplicationTable
    putStrLn $ "Surviving mutants: " ++ show survivors

    let props1 = [prop_tenElements]
        props2 = [prop_firstElementIsInput]
        props3 = [prop_sumIsTriangleNumberTimesInput]
        props4 = [prop_linear]
        props5 = [prop_moduloIsZero]

    survivors1 <- countSurvivors 4000 mutators props1 multiplicationTable
    putStrLn $ "Surviving mutants Props1: " ++ show survivors1
    survivors2 <- countSurvivors 4000 mutators props2 multiplicationTable
    putStrLn $ "Surviving mutants Props2: " ++ show survivors2
    survivors3 <- countSurvivors 4000 mutators props3 multiplicationTable
    putStrLn $ "Surviving mutants Props3: " ++ show survivors3
    survivors4 <- countSurvivors 4000 mutators props4 multiplicationTable
    putStrLn $ "Surviving mutants Props4: " ++ show survivors4
    survivors5 <- countSurvivors 4000 mutators props5 multiplicationTable
    putStrLn $ "Surviving mutants Props5: " ++ show survivors5

    let numberOfMutants = 4000

    results <- mapM
        (\(mutantSetName, selectedMutators) -> do
            cells <- mapM
                (\(_, selectedProperties) -> do
                    survivors <- countSurvivors
                        numberOfMutants
                        selectedMutators
                        selectedProperties
                        multiplicationTable

                    pure (formatKillResult numberOfMutants survivors)
                )
                propertySets

            pure (mutantSetName : cells)
        )
        mutantSets

    let header = "Mutant set" : map fst propertySets

    putStrLn "\nMutation-testing results"
    putStrLn "Each cell: killed/total (kill percentage)"
    putStrLn $ renderTable (header : results)

