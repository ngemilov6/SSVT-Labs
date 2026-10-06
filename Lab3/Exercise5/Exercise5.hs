module Exercise5 where

import MultiplicationTable
import Mutation
import Exercise2
import Exercise3
import Control.Monad (replicateM)
import Data.List (isSubsequenceOf, subsequences)
import Test.QuickCheck

--- Data type for representing conjectures
data Conjecture
    = Equivalent Int Int
    | FollowsFrom Int [Int]
    deriving Show

--- Find equivalent properties if their mutation results are the same for all mutants
equivalentProperties :: Int -> Int -> [[Bool]] -> Bool
equivalentProperties property1 property2 mutationResults =
    all (\results -> results !! property1 == results !! property2)
    mutationResults

--- Find implications between properties based on mutation results
impliedBy :: Int -> [Int] -> [[Bool]] -> Bool
impliedBy target antecedents mutationResults =
    all
        (\results ->
            let antecedentsHold = all (results !!) antecedents
                targetHolds = results !! target
            in not antecedentsHold || targetHolds
        )
        mutationResults

--- Find the minimal implication sets for a shown implication relationship, ensuring no smaller subset implies the target property
minimalImplicationSets :: Int -> [[Bool]] -> [[Int]]
minimalImplicationSets target mutationResults =
    let propertyCount = length (head mutationResults)
        otherProperties = filter (/= target) [0 .. propertyCount - 1]
        implicationSets = filter (\antecedents -> impliedBy target antecedents mutationResults) (subsequences otherProperties)
    in filter
        (\antecedents -> not (any (\smaller -> strictSubsetOf smaller antecedents)implicationSets))
        implicationSets

--- Find both equivalences and implications between properties based on the mutation testing results
findConjectures :: Int -> [Mutator] -> [MProperty] -> (Integer -> [Integer]) -> IO [Conjecture]
findConjectures mutantCount mutators properties functionUnderTest
    | mutantCount <= 0 = pure []
    | null properties = pure []
    | otherwise = do
        mutationResults <-
            replicateM
                mutantCount
                (generateMutationResults mutators properties functionUnderTest)
        let propertyIndices = [0 .. length properties - 1]
            equivalences =
                [ Equivalent (property1 + 1) (property2 + 1)
                | property1 <- propertyIndices
                , property2 <- [property1 + 1 .. length properties - 1]
                , equivalentProperties property1 property2 mutationResults
                ]
            implications =
                [ FollowsFrom (target + 1) (map (+ 1) antecedents)
                | target <- propertyIndices
                , antecedents <- minimalImplicationSets target mutationResults
                , not (null antecedents)
                , length antecedents > 1
                ]
        pure (equivalences ++ implications)

--- identical propety to property 1 (ten elements)
prop_tenElements2 :: [Integer] -> Integer -> Bool
prop_tenElements2 o i = length o == 10

main :: IO ()
main = do
    conjectures <-
        findConjectures
            1000000
            mutators
            (multiplicationTableProps ++ [prop_tenElements2])
            multiplicationTable
    mapM_ print conjectures