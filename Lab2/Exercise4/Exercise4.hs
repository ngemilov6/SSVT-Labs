type Rel a = [(a,a)]

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial _ [] = True
isSerial [] _ = True
