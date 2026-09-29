import Control.Monad (forM_, when)
import Data.Array.ST (newArray, writeArray, runSTUArray)
import Data.Array.Unboxed (UArray, (!), assocs)
import Test.QuickCheck

-- Segmented sieve: primality of every number in [lo..hi]
sieveRange :: Int -> Int -> UArray Int Bool
sieveRange lo hi = runSTUArray $ do
    arr <- newArray (lo, hi) True
    when (lo <= 1) $ forM_ [lo .. min 1 hi] $ \i -> writeArray arr i False
    forM_ (smallPrimes (isqrt hi)) $ \p -> do
        let start = max (p * p) (((lo + p - 1) `div` p) * p)
        forM_ [start, start + p .. hi] $ \i -> writeArray arr i False
    return arr

smallPrimes :: Int -> [Int]
smallPrimes m
    | m < 2 = []
    | otherwise = [i | (i, True) <- assocs (sieveRange 2 m)]

isqrt :: Int -> Int
isqrt n = floor (sqrt (fromIntegral n :: Double))

-- Calculate the value at a specific row and column
valAt :: Int -> Int -> Int
valAt r c
    | c >= 1 && c <= r = (r * (r - 1)) `div` 2 + c
    | otherwise = 0

-- Get the valid (row, col) coordinates of all up to 8 neighbours
neighbours :: (Int, Int) -> [(Int, Int)]
neighbours (r, c) = filter isValid [ (r-1, c-1), (r-1, c), (r-1, c+1), (r, c-1), (r, c+1), (r+1, c-1), (r+1, c), (r+1, c+1)]
  where
    isValid (nr, nc) = nc >= 1 && nc <= nr

-- Given a fast localized primality test, check if (r,c) is the center of a triplet
isCenter :: ((Int, Int) -> Bool) -> (Int, Int) -> Bool
isCenter isP coord =
    let primeNeighbours = filter isP (neighbours coord)
    in length primeNeighbours >= 2

-- Check if a prime at (r,c) is part of any prime triplet
inPrimeTriplet :: ((Int, Int) -> Bool) -> (Int, Int) -> Bool
inPrimeTriplet isP coord =
    isCenter isP coord || any (isCenter isP) (filter isP (neighbours coord))

-- Calculate S(n): The sum of primes in row n that are in a prime triplet
-- Only rows n-2 .. n+2 are ever inspected, so we sieve just that range.
s :: Int -> Int
s n = sum [valAt n c | c <- [1..n], isP (n, c), inPrimeTriplet isP (n, c)]
  where
    lo = valAt (max 1 (n - 2)) 1
    hi = valAt (n + 2) (n + 2)
    sieve = sieveRange lo hi
    isP (r, c) = let v = valAt r c in v >= lo && v <= hi && sieve ! v


-- 1. Property: Bounds Checking
-- Neighbours must always fall within the strict bounds of the triangle (1 <= c <= r)
prop_validNeighbourBounds :: Positive Int -> Positive Int -> Property
prop_validNeighbourBounds (Positive r) (Positive c) =
    c <= r ==>
        let ns = neighbours (r, c)
        in all (\(nr, nc) -> nc >= 1 && nc <= nr) ns

-- 2. Property: Neighbour Symmetry
-- If A is a neighbour of B, then B must be a neighbour of A.
prop_neighbourSymmetry :: Positive Int -> Positive Int -> Property
prop_neighbourSymmetry (Positive r) (Positive c) =
    c <= r ==>
        let coord = (r, c)
            ns = neighbours coord
        in all (\n -> coord `elem` neighbours n) ns

-- 3. Property: Base Cases from Project Euler Description
prop_eulerBaseCases :: Property
prop_eulerBaseCases =
    (s 8 === 60) .&&. (s 9 === 37) .&&. (s 10000 === 950007619)

runTests :: IO ()
runTests = do
    putStrLn "Testing Neighbour Bounds constraint:"
    quickCheck prop_validNeighbourBounds
    putStrLn "Testing Neighbour Spatial Symmetry:"
    quickCheck prop_neighbourSymmetry
    putStrLn "Testing Project Euler base cases:"
    quickCheck (once prop_eulerBaseCases)

main :: IO ()
main = do
    runTests
    print (s 5678027 + s 7208785)
