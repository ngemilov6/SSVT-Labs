euler9 :: Integer
euler9 = head [ a * b * c | a <- [1..333], b <- [a+1..499], let c = 1000 - a - b , a^2 + b^2 == c^2 ]