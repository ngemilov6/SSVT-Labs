-- ---------------------------------------------------------------
-- Lecture 1 -- Haskell and Logic
-- Software Specification, Verification and Testing
--
-- All the code from the slides, in the same order, so you can
-- follow along in GHCi:
--
--     ghci Lecture1.hs
--
-- Reading: chapters 1 and 2 of The Haskell Road.
--
-- NOTE ON NAMES. The slides show definitions of take, map, filter
-- and foldr under their real names. Here they are called myTake,
-- myMap, myFilter and myFoldr, because the Prelude already
-- defines the originals and we want both around: the library
-- version is the oracle we test ours against.
-- ---------------------------------------------------------------

module Lecture1 where

import Data.List
import Test.QuickCheck

-- ===============================================================
-- 1. LAZY LISTS
-- ===============================================================

-- Nothing is computed until someone asks. Try:  take 65 sentence
sentence :: String
sentence = "Sentences can go " ++ onAndOn

onAndOn :: String
onAndOn = "on and " ++ onAndOn

-- A list whose definition mentions the list itself. Each element
-- only needs the previous one, so the recursion is productive.
-- Try:  take 4 sentences
sentences :: [String]
sentences = "(Large) Sentences can go on"
            : map (++ " and on") sentences

-- Our standard infinite domain for the rest of the lecture.
nats :: [Integer]
nats = [0..]

-- --- the recursive definitions, spelled out -------------------
-- Note that all three produce the head of the result BEFORE
-- recursing: that is exactly why they work on infinite lists.

myTake :: Int -> [a] -> [a]
myTake 0 _      = []              -- asked for nothing
myTake _ []     = []              -- nothing left to give
myTake n (x:xs) = x : myTake (n-1) xs

myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs) | p x       = x : myFilter p xs
                  | otherwise =     myFilter p xs

-- ===============================================================
-- 2. PROPERTIES AS PREDICATES
--
-- A property of things of type a IS a function a -> Bool.
-- ===============================================================

threefold :: Integer -> Bool
threefold n = rem n 3 == 0

-- Try:  take 6 threefolds
threefolds :: [Integer]
threefolds = filter threefold nats

-- ===============================================================
-- 3. QUANTIFIERS
-- ===============================================================

-- Both statements below are TRUE, and both DIVERGE.
--   any p xs terminates only when the answer is True;
--   all p xs terminates only when the answer is False.
-- Each of these starts with the quantifier that cannot confirm
-- a truth, so neither ever answers. Do not evaluate them.

existsLargestNatural :: Bool
existsLargestNatural  = all (\n -> any (\m -> n <  m) nats) nats

existsSmallestNatural :: Bool
existsSmallestNatural = any (\n -> all (\m -> n <= m) nats) nats

-- flip swaps the two arguments of a function; nothing is
-- computed, flip f is just another function.
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

-- With the arguments in the other order, the code reads like the
-- formula: "for all n in nats, ...".
--
-- The primes in the names are forced on us: since GHC 9.10
-- 'forall' is a reserved keyword and can no longer be used as an
-- identifier. Older material (The Haskell Road included) writes
-- these two without the prime, and no longer compiles.
forall' :: [a] -> (a -> Bool) -> Bool
forall' = flip all

exist' :: [a] -> (a -> Bool) -> Bool
exist' = flip any

existsLargestNatural' :: Bool
existsLargestNatural'  = forall' nats (\n -> exist'  nats (\m -> n <  m))

existsSmallestNatural' :: Bool
existsSmallestNatural' = exist'  nats (\n -> forall' nats (\m -> n <= m))

-- The one logical operator the Prelude does not provide.
-- infix 1 = lower precedence than everything else, so
--   p && q --> r   means   (p && q) --> r
infix 1 -->

(-->) :: Bool -> Bool -> Bool
p --> q = not p || q

-- ===============================================================
-- 4. TESTING WITH QUICKCHECK
-- ===============================================================

myall :: (a -> Bool) -> [a] -> Bool
myall _ []     = True                 -- vacuously true
myall p (x:xs) = p x && myall p xs

-- The obvious property is
--     \p xs -> all p xs == myall p xs
-- but QuickCheck cannot use it: to print a counterexample it
-- needs every generated argument to be Show-able, and there is
-- no way to print a function.
--
--   > quickCheckResult (\p xs -> all p xs == myall p xs)
--   No instance for (Show (() -> Bool))
--
-- So we generate DATA and build the predicate from it: a list
-- determines the predicate "is it in this list?".

list2p :: Eq a => [a] -> a -> Bool
list2p = flip elem

-- Specialising to [Int] is necessary: QuickCheck has to know
-- which type to generate.
myallTest :: [Int] -> [Int] -> Bool
myallTest = \ys xs -> let p = list2p ys in
              all p xs == myall p xs

-- > quickCheck myallTest
-- +++ OK, passed 100 tests.

-- --- the recursion pattern, made explicit ---------------------
-- z is the answer for the empty list; f says how to combine the
-- head with the answer for the tail. Write those two and you
-- have written the recursion.

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ z []     = z
myFoldr f z (x:xs) = f x (myFoldr f z xs)

-- myall again, with the two ingredients passed as arguments.
myall' :: (a -> Bool) -> [a] -> Bool
myall' p = foldr (\x b -> p x && b) True

myallTest' :: [Int] -> [Int] -> Bool
myallTest' = \ys xs -> let p = list2p ys in
               all p xs == myall' p xs

-- > quickCheck myallTest'
-- +++ OK, passed 100 tests.

-- ===============================================================
-- 5. A WORKED EXAMPLE: PRIMES
-- ===============================================================

-- The specification, in predicate logic:
--   P(n) :== n in N  /\  n > 1  /\
--            forall d in N (1 < d < n -> not D(d,n))
--
-- Implement the vocabulary before the sentence: D(d,n) first.

divide :: Integer -> Integer -> Bool
divide n m = rem m n == 0          -- "n divides m"

-- Now the formula transcribes symbol by symbol. The quantifier
-- is bounded, so the list is finite and this terminates.
-- Obviously correct, and obviously slow: n-2 divisions.
isPrime :: Integer -> Bool
isPrime n = n > 1 && all (\d -> not (divide d n)) [2..n-1]

-- A composite number always has a divisor at or below its square
-- root: if n = a*b with 1 < a <= b < n, then a^2 <= a*b = n.
-- So O(n) divisions become O(sqrt n).
--
-- takeWhile stops at the FIRST element that fails the test --
-- unlike filter, which would run forever on [2..].
isPrime' :: Integer -> Bool
isPrime' n = all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) [2..]

-- Faster still: if n has a divisor at all, it has a PRIME
-- divisor, so trial division by 4, 6, 8, 9, ... was redundant.
--
-- prime and primes refer to each other, and it terminates: to
-- decide 'prime n' we only consult primes up to sqrt n, all of
-- them strictly smaller than n, hence already produced.
prime :: Integer -> Bool
prime n = n > 1 && all (\x -> rem n x /= 0) xs
  where xs = takeWhile (\y -> y^2 <= n) primes

primes :: [Integer]
primes = 2 : filter prime [3..]

-- A different algorithm: no divisibility test at all. The head
-- is prime by construction; remove its multiples and repeat.
sieve :: [Integer] -> [Integer]
sieve (n:ns) = n : sieve (filter (\m -> rem m n /= 0) ns)
sieve []     = []

eprimes :: [Integer]
eprimes = sieve [2..]

-- Two independent implementations of one specification: that is
-- a test waiting to be written. We cannot compare two infinite
-- lists, so we compare finite prefixes.
--
-- The --> guards against the negative values QuickCheck will
-- certainly try. Careful: a property whose precondition is
-- almost never satisfied passes without testing anything.
primesAgree :: Int -> Bool
primesAgree n = n >= 0 --> take n primes == take n eprimes

-- > quickCheck primesAgree
-- Try it, then read the exercise at the bottom of this file.

-- ===============================================================
-- 6. OPTIONAL EXERCISES (not graded, not covered in the lecture)
-- ===============================================================

-- (a) Write the types of not, (&&), (||), (-->), all, any from
--     memory, then check them in GHCi with :t
--
-- (b) QuickCheck your own definitions against the library ones.
--     The first three are done for you; the property is always
--     the same shape: "same inputs, same output".

-- The precondition is not decoration: myTake, unlike the
-- Prelude's take, does not handle a negative count. Without the
-- guard this property fails -- which is the point of writing it.
prop_myTake :: Int -> [Int] -> Bool
prop_myTake n xs = n >= 0 --> myTake n xs == take n xs

prop_myFilter :: [Int] -> [Int] -> Bool
prop_myFilter ys xs = let p = list2p ys in
                        myFilter p xs == filter p xs

prop_myFlip :: Int -> Int -> Bool
prop_myFlip x y = myFlip (-) x y == flip (-) x y

-- Note that myMap and myFoldr cannot be tested this way for the
-- same reason myall could not: their first argument is a
-- function. Fix it the way we fixed myall -- generate data and
-- build the function from it.
--
-- (c) Write a property saying that eprimes and primes have the
--     same content, then say what your property does NOT say.
--     What happens to primesAgree with n = 10000? The test still
--     passes. Does it still finish?
--
-- (d) Once a property is a value, functions can TAKE a property
--     as input. 'least' returns the smallest natural number
--     satisfying p -- read it as a definition, not as a search:
--     "the first of the naturals that satisfy p". Laziness turns
--     the definition into the search. If no number satisfies p,
--     it runs forever, which is the 'any' case from the lecture
--     wearing a different hat.
--
--     (GHC warns that head is partial. On an infinite list it is
--     not: the list is never empty. It just may never answer.)

least :: (Integer -> Bool) -> Integer
least p = head (filter p nats)

--     Write 'least' again as an explicit loop, without filter,
--     and QuickCheck the two against each other on properties
--     you know are satisfiable -- e.g. (\n -> n*n > k).
