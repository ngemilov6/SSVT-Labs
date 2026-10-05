module Exercise4 where

propertyStrength :: Integer -> Integer -> Double
propertyStrength validMutants survivors
    | validMutants <= 0 = 0.0
    | otherwise = 
        let killed = validMutants - survivors
        in (fromIntegral killed / fromIntegral validMutants) * 100.0