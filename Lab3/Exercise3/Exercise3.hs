module Exercise3 where

import Control.Monad (replicateM, forM_)
import Data.List (isSubsequenceOf, subsequences, intercalate)
import MultiplicationTable
import Mutation
import Test.QuickCheck
import Exercise2

type MProperty = [Integer] -> Integer -> Bool
type Mutator = [Integer] -> Gen [Integer]

--- Generate the outcomes of all properties for one changed mutant
generateMutationResults :: [Mutator] -> [MProperty] -> (Integer -> [Integer]) -> IO [Bool]
generateMutationResults mutators properties functionUnderTest = do
    mutator <- generate (elements mutators)
    input <- generate (arbitrary :: Gen Integer)
    results <- generate (mutate' mutator properties functionUnderTest input)

    --- regenerate if mutation is equivalent
    if null results
        then generateMutationResults mutators properties functionUnderTest
        else pure results

--- A property kills a mutant when it returns False
killsMutant :: [Int] -> [Bool] -> Bool
killsMutant selected results = any (not . (results !!)) selected

--- The complete property set kills a mutant if at least one property rejects it
fullSetKillsMutant :: [Bool] -> Bool
fullSetKillsMutant results =
    any not results

strictSubsetOf :: (Eq a) => [a] -> [a] -> Bool
strictSubsetOf xs ys = xs `isSubsequenceOf` ys && length xs < length ys
    
--- Return every minimal subset of properties that kills the same generated mutants as the full property set
minimalPropertySubsets :: Int -> [MProperty] -> (Integer -> [Integer]) -> IO [[Int]]
minimalPropertySubsets mutantCount properties functionUnderTest
    | null properties = pure [[]]
    | otherwise = do
        mutationResults <-
            replicateM
                (max 0 mutantCount)
                (generateMutationResults mutators properties functionUnderTest)

        let propertyIndices = [0 .. length properties - 1]
            allEffectiveSubsets =
                filter (\subset -> all (\results -> killsMutant subset results == fullSetKillsMutant results) mutationResults) (subsequences propertyIndices)
            --- minimal subset if no subset of it kills the same mutatnts
            minimalIndexSubsets =
                filter
                    (\subset -> not (any (\smaller -> strictSubsetOf smaller subset) allEffectiveSubsets))
                    allEffectiveSubsets
            
            smallestSize = minimum (map length allEffectiveSubsets)
            smallestIndexSubsets =
                filter
                    ((== smallestSize) . length)
                    allEffectiveSubsets
        pure smallestIndexSubsets

namedProperties :: [(String, MProperty)]
namedProperties =
    [ ("P1: ten elements", prop_tenElements)
    , ("P2: first element", prop_firstElementIsInput)
    , ("P3: triangle sum", prop_sumIsTriangleNumberTimesInput)
    , ("P4: linear", prop_linear)
    , ("P5: modulo zero", prop_moduloIsZero)
    ]

main :: IO ()
main = do
    minimalSubsets <- minimalPropertySubsets 1000000 multiplicationTableProps multiplicationTable

    let propertyNames =
            map fst namedProperties

    putStrLn $
        "Number of minimal property subsets: "
            ++ show (length minimalSubsets)

    putStrLn "Minimal property subsets:"

    forM_ minimalSubsets $ \subset ->
        putStrLn $
            "  {"
                ++ intercalate
                    ", "
                    [propertyNames !! index | index <- subset]
                ++ "}"

    putStrLn $ "Number of minimal property subsets: " ++ show (length minimalSubsets)
    putStrLn "Minimal property subsets:"

    putStrLn "Number of properties in each minimal subset:"
    print (map length minimalSubsets)
    