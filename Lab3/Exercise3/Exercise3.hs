module Exercise3 where

import Control.Monad (replicateM)
import Data.List (isSubsequenceOf, subsequences)
import MultiplicationTable
import Mutation
import Test.QuickCheck

type MProperty = [Integer] -> Integer -> Bool

--- Generate the outcomes of all properties for one changed mutant
generateMutationResults :: [MProperty] -> (Integer -> [Integer]) -> IO [Bool]
generateMutationResults properties functionUnderTest = do
    mutator <- generate (elements mutators)
    input <- generate (arbitrary :: Gen Integer)
    results <- generate (mutate' mutator properties functionUnderTest input)

    --- regenerate if mutation is equivalent
    if null results
        then generateMutationResults properties functionUnderTest
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
minimalPropertySubsets :: Int -> [MProperty] -> (Integer -> [Integer]) -> IO [[MProperty]]
minimalPropertySubsets mutantCount properties functionUnderTest
    | null properties = pure [[]]
    | otherwise = do
        mutationResults <-
            replicateM
                (max 0 mutantCount)
                (generateMutationResults properties functionUnderTest)

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
        pure[[properties !! index | index <- subset]| subset <- smallestIndexSubsets]

main :: IO ()
main = do
    minimalSubsets <- minimalPropertySubsets 4000 multiplicationTableProps multiplicationTable

    putStrLn $ "Number of minimal property subsets: " ++ show (length minimalSubsets)
    putStrLn "Minimal property subsets:"

    putStrLn "Number of properties in each minimal subset:"
    print (map length minimalSubsets)