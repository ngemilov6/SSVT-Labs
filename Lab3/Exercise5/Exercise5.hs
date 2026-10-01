module Exercise5 where

import MultiplicationTable

sampleInputs :: Integer -> [Integer]
sampleInputs n = [0 .. max 0 (n - 1)]

propertyNames :: [String]
propertyNames =
    [ "prop_tenElements"
    , "prop_firstElementIsInput"
    , "prop_sumIsTriangleNumberTimesInput"
    , "prop_linear"
    , "prop_moduloIsZero"
    ]

-- Function to evaluate one property over a sample of inputs
-- This returns a list of booleans, showing whether the property holds for that input
propertyResults :: (Integer -> [Integer]) -> ([Integer] -> Integer -> Bool) -> Integer -> [Bool]
propertyResults functionUnderTest property sampleSize =
    [ property (functionUnderTest input) input
    | input <- sampleInputs sampleSize
    ]

-- Function to collect the inputs for which a property holds
-- These are the valid cases of that property over the chosen sample
propertyCases :: (Integer -> [Integer]) -> ([Integer] -> Integer -> Bool) -> Integer -> [Integer]
propertyCases functionUnderTest property sampleSize =
    [ input
    | input <- sampleInputs sampleSize
    , property (functionUnderTest input) input
    ]

-- Function to find equivalent properties on the sampled inputs
-- Two properties are equivalent if they produce the same truth value for every input in the sample
equivalentProperties :: (Integer -> [Integer]) -> [([Integer] -> Integer -> Bool)] -> Integer -> [(Int, Int)]
equivalentProperties functionUnderTest properties sampleSize =
    [ (i, j)
    | i <- [0 .. length properties - 1]
    , j <- [i + 1 .. length properties - 1]
    , propertyResults functionUnderTest (properties !! i) sampleSize
      == propertyResults functionUnderTest (properties !! j) sampleSize
    ]

-- Function to find subset relationships between properties
-- A property is a subset of another when every input that satisfies the first also satisfies the second
subsetProperties :: (Integer -> [Integer]) -> [([Integer] -> Integer -> Bool)] -> Integer -> [(Int, Int)]
subsetProperties functionUnderTest properties sampleSize =
    [ (i, j)
    | i <- [0 .. length properties - 1]
    , j <- [0 .. length properties - 1]
    , i /= j
    , all (\input -> input `elem` propertyCases functionUnderTest (properties !! j) sampleSize)
           (propertyCases functionUnderTest (properties !! i) sampleSize)
    ]

-- Function to find superset relationships between properties
-- This is the reverse of subsetProperties; if property i contains all the sampled
-- cases of property j, then i is a superset of j
supersetProperties :: (Integer -> [Integer]) -> [([Integer] -> Integer -> Bool)] -> Integer -> [(Int, Int)]
supersetProperties functionUnderTest properties sampleSize =
    [ (i, j)
    | (j, i) <- subsetProperties functionUnderTest properties sampleSize
    ]

main :: IO ()
main = do
    let sampleSize = 20
    putStrLn "Properties in order:"
    mapM_ putStrLn (zipWith (\index name -> show index ++ ": " ++ name) [0 ..] propertyNames)
    putStrLn "Equivalent property pairs:"
    print (equivalentProperties multiplicationTable multiplicationTableProps sampleSize)
    putStrLn "Subset relations (property i subset of property j):"
    print (subsetProperties multiplicationTable multiplicationTableProps sampleSize)
    putStrLn "Superset relations (property i superset of property j):"
    print (supersetProperties multiplicationTable multiplicationTableProps sampleSize)

