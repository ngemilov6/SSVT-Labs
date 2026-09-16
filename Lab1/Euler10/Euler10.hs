primes :: [Integer]
primes = 2 : 3 : filter isPrime [5,7..]
  where
    isPrime n = foldr (\p r -> p*p > n || ((n `rem` p) /= 0 && r)) True primes

euler10 :: Integer
euler10 = sum $ takeWhile (< 2000000) primes