module Exercise3 where

import Data.List (subsequences)
import MultiplicationTable
import Exercise2 hiding (main)
import Test.QuickCheck

-- run with: runghc --ghc-arg=-i../:../Exercise2 Exercise3.hs +RTS -M512M -RTS

-- A minimal property subset is a subset that satisfies the property under test,
-- and no proper subset of it satisfies the same property.
minimalPropertySubsets :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO [[([Integer] -> Integer -> Property)]]
minimalPropertySubsets testCount properties functionUnderTest = do
    candidateResults <- mapM evaluateSubset (filter (not . null) (subsequences [0 .. length properties - 1]))
    let minimalSubsets = [ map (properties !!) indices
                        | (indices, satisfiesProperty) <- candidateResults
                        , satisfiesProperty
                        , not (any (\(otherIndices, otherSatisfiesProperty) ->
                                      otherSatisfiesProperty && properSubsetOf otherIndices indices) candidateResults)
                        ]
    return minimalSubsets
  where
    evaluateSubset indices = do
        subsetPasses <- subsetSatisfies testCount (map (properties !!) indices) functionUnderTest
        return (indices, subsetPasses)

properSubsetOf :: Eq a => [a] -> [a] -> Bool
properSubsetOf smallerList largerList = smallerList /= largerList && all (`elem` largerList) smallerList

subsetSatisfies :: Integer -> [([Integer] -> Integer -> Property)] -> (Integer -> [Integer]) -> IO Bool
subsetSatisfies count subset fut = do
    let inputs = [0 .. max 0 (count - 1)]
    results <- mapM (\input -> do
        let output = fut input
        all isSuccess <$> traverse
            (\propertyUnderTest -> quickCheckWithResult (stdArgs { maxSuccess = 1, chatty = False }) (propertyUnderTest output input))
            subset
        ) inputs
    return (and results)

main :: IO ()
main = do
    -- Print the sizes of the minimal property subsets.
    -- Property function values do not have a Show instance.
    subsets <- minimalPropertySubsets 10 multiplicationTableProperties multiplicationTable
    print (map length subsets)

