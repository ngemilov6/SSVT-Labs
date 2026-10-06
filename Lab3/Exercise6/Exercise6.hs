module Exercise6 where

import Exercise5 (findConjectures, Conjecture(..), prop_tenElements2)
import MultiplicationTable
import Mutation
import Exercise3
import Exercise2 (countSurvivors)
import Data.List (intercalate)
import Test.QuickCheck
import Data.List (intercalate, transpose)
import Text.Printf (printf)
import Control.Monad (forM_)



antecedentsToString :: [Int] -> String
antecedentsToString antecedents = intercalate ", " (map show antecedents)

printConjectures :: Int -> [MProperty] -> (Integer-> [Integer]) -> IO ()
printConjectures mutantCount properties functionUnderTest = do
    conjectures <-
        findConjectures
            mutantCount
            mutators
            properties
            functionUnderTest
    putStrLn "Conjectures:"
    mapM_ printConjecture conjectures
    where 
        printConjecture (Equivalent p1 p2) =
            putStrLn $ "{" ++ show p1 ++ "} <=> {" ++ show p2 ++ "}"
        printConjecture (FollowsFrom target antecedents) =
            putStrLn $ "{" ++ show target ++ "} <= {" ++ antecedentsToString antecedents ++ "}"


printKillRates :: Int -> [(String,[MProperty])] -> (Integer-> [Integer]) -> IO ()
printKillRates mutantCount propertiesSet functionUnderTest = do
    let intMutantCount = fromIntegral mutantCount
    printTableOfResults intMutantCount mutantSets propertiesSet functionUnderTest

printMinimalPropertySubsets :: Int -> [(String,[MProperty])] -> (Integer-> [Integer]) -> IO ()
printMinimalPropertySubsets mutantCount propertiesSet functionUnderTest = do
    minimalSubsets <- minimalPropertySubsets mutantCount [property | (name, [property]) <- propertiesSet] functionUnderTest
    putStrLn $
        "\nNumber of minimal property subsets: "
            ++ show (length minimalSubsets)

    putStrLn "Minimal property subsets:"

    forM_ minimalSubsets $ \subset ->
        putStrLn $
            "  {"
                ++ intercalate
                    ", "
                    [fst (propertiesSet !! index) | index <- subset]
                ++ "}"

    putStrLn $ "Number of minimal property subsets: " ++ show (length minimalSubsets)
    putStrLn "Minimal property subsets:"

    putStrLn "Number of properties in each minimal subset:"
    print (map length minimalSubsets)

createReport :: Int-> [(String,[MProperty])] -> (Integer-> [Integer]) -> IO ()
createReport mutantCount properties functionUnderTest = do
    printKillRates mutantCount properties functionUnderTest
    printConjectures mutantCount [property | (name, [property]) <- properties] functionUnderTest
    printMinimalPropertySubsets mutantCount (drop 1 properties) functionUnderTest

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

mutantSets =
    [ ("All mutators", mutators)
    , ("M1: anyList", [anyList])
    , ("M2: removeElements", [removeElements])
    , ("M3: addElements", [addElements])]


printTableOfResults :: Integer -> [(String, [[Integer] -> Gen [Integer]])] -> [(String, [([Integer] -> Integer -> Bool)])] -> (Integer -> [Integer]) -> IO ()
printTableOfResults numberOfMutants mutatorsSet propertiesSet functionUnderTest = do
    results <- mapM
        (\(mutantSetName, selectedMutators) -> do
            cells <- mapM
                (\(_, selectedProperties) -> do
                    survivors <- countSurvivors
                        numberOfMutants
                        selectedMutators
                        selectedProperties
                        functionUnderTest
                    pure (formatKillResult numberOfMutants survivors)
                )
                propertiesSet
            pure (mutantSetName : cells)
        )
        mutatorsSet
    let header = "Mutant set" : map fst propertiesSet
    putStrLn "\nMutation-testing results"
    putStrLn "Each cell: killed/total (kill percentage)"
    putStrLn $ renderTable (header : results)

main :: IO ()
main = do
    let propertiesToTest = [ ("All properties", multiplicationTableProps)
                            , ("P1: ten elements", [prop_tenElements])
                            , ("P2: first element", [prop_firstElementIsInput])
                            , ("P3: triangle sum", [prop_sumIsTriangleNumberTimesInput])
                            , ("P4: linear", [prop_linear])
                            , ("P5: modulo zero", [prop_moduloIsZero])
                            , ("P6: ten elements 2", [prop_tenElements2])
                            ]
    createReport
        100000
        propertiesToTest
        multiplicationTable
