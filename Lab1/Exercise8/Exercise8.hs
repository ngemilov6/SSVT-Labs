module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck
import Lecture3

import SetOrd

-- run file with: 
-- runghc --ghc-arg=-i.. Exercise8.hs +RTS -M512M -RTS


sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
-- NOTE: The datatype uses n-ary conjunctions/disjunctions(lists), so
-- your implementation should handle <code>Cnj fs</code> and <code>Dsj fs</code> for any list length.
-- (The binary-only version would miss sub-formulae for lists longer than 2.)
sub f@(Cnj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Dsj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

-- property to check if return from sub() contains the original formula
prop_subInlcudesFormula :: Form -> Bool
prop_subInlcudesFormula f = let Set fs = sub f in f `elem` fs

-- property to check if return from sub() contains no duplicates
prop_subIncludeNoDuplicates :: Form -> Bool
prop_subIncludeNoDuplicates f = let Set fs = sub f in length fs == length (nub fs)


nsub :: Form -> Int
-- use sub() implementation to count the number of sub-formulae in a formula
nsub f = countSet (sub f)
    where
        -- egdecase: empty set
        countSet (Set []) = 0
        -- recursive counting of sub-sets
        countSet (Set (_ : fs)) = 1 + countSet (Set fs)

-- quickcheck generator type for Form
instance Arbitrary Form where arbitrary = sized genForm

genForm :: Int -> Gen Form
genForm 0 =
  Prop <$> choose (0, 10)

genForm n =
  oneof
    [ Prop <$> choose (0, 10), 
    Neg <$> genForm (n - 1), 
    Impl <$> genForm (n `div` 2) <*> genForm (n `div` 2), 
    Equiv <$> genForm (n `div` 2) <*> genForm (n `div` 2)]

-- property to check if nsub() returns same length and set of all sub-formulae
prop_nsubLength :: Form -> Bool
prop_nsubLength f = let Set fs = sub f in nsub f == length fs

main :: IO ()
main = do 
    quickCheck prop_subInlcudesFormula
    quickCheck prop_subIncludeNoDuplicates
    quickCheck prop_nsubLength