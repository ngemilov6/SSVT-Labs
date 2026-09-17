{-# LANGUAGE NoExplicitForAll #-}

module Exercise8 where

import Data.List
import System.Random
import Test.QuickCheck
import SetOrd
-- import Lecture1
-- import Lecture2
-- import Lecture3


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- forall :: [a] -> (a -> Bool) -> Bool
-- forall = flip all
---

type Name = Int
data Form = 
  Prop Name
  | Neg Form
  | Cnj [Form]
  | Dsj [Form]
  | Impl Form Form
  | Equiv Form Form
  deriving (Eq,Ord)

instance Show Form where
  show (Prop x) = show x
  show (Neg f) = '-' : show f
  show (Cnj fs) = "*(" ++ showLst fs ++ ")"
  show (Dsj fs) = "+(" ++ showLst fs ++ ")"
  show (Impl f1 f2) =
    "(" ++ show f1 ++ "==>"
      ++ show f2
      ++ ")"
  show (Equiv f1 f2) =
    "(" ++ show f1 ++ "<=>"
      ++ show f2
      ++ ")"

showLst, showRest :: [Form] -> String
showLst [] = ""
showLst (f : fs) = show f ++ showRest fs
showRest [] = ""
showRest (f : fs) = ' ' : show f ++ showRest fs

sub :: Form -> Set Form
sub (Prop x) = Set [Prop x]
sub (Neg f) = unionSet (Set [Neg f]) (sub f)
sub f@(Cnj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Dsj fs) = foldl unionSet (Set [f]) (map sub fs)
sub f@(Impl f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)
sub f@(Equiv f1 f2) = unionSet ( unionSet (Set [f]) (sub f1)) (sub f2)

--- NEEDS REWORKING, NOT ELIMINATING DOUBLES
nsub :: Form -> Int
nsub (Prop x) = 1
nsub (Neg f) = 1 + (nsub f)
nsub (Cnj fs) = 1 + (sum (map nsub fs))
nsub (Dsj fs) = 1 + (sum (map nsub fs))
nsub (Impl f1 f2) = 1 + (nsub f1) + (nsub f2)
nsub (Equiv f1 f2) = 1 + (nsub f1) + (nsub f2)

len_set :: Set Form -> Int
len_set (Set xs) = length xs

prop_correct_length :: Form -> Bool
prop_correct_length form = len_set (sub form) == nsub form

examples :: [Form]
examples = [Equiv (Impl (Prop 1) (Prop 2)) (Dsj [Neg (Prop 3), Prop 4]),
  Dsj [Cnj [Prop 1, Prop 2], Cnj [Neg (Prop 3), Prop 4]],
  Dsj [Prop 1, Cnj [Prop 2, Dsj [Neg (Prop 3), Cnj[Prop 4, Prop 5]]]],
  Dsj [Dsj [Prop 1, Cnj [Prop 2, Neg (Prop 3)]], Cnj[Prop 4, Prop 5]],
  Dsj [Cnj[Prop 1, Neg (Prop 2)], Cnj[Neg (Prop 3), Prop 4], Cnj[Prop 5, Prop 6]]]

main :: IO()
main = do 
  let example = Cnj[Dsj[Prop 1, Neg (Prop 2)], Prop 3]
  print (show (sub example))
  print (nsub example)
  print (all (\ex -> prop_correct_length ex) examples)
    


{- Time taken: 35min (Question 1); 15min (Question 2)
1. We can prove the correctness by induction on the complexity of the formula. For this, we assume that lower order
functions are correct, such as unionSet, foldl, map, and the implementation of Set.

Base Case: Prop x, we will call this complexity 0, as the fomula tree has depth 0.
sub (Prop x) = Set [Prop x], which is correct as there is only one subformula for Prop x.

Inductive Assumption:
We assume that for all formulas of a complexity n, the sub implementation is correct.

Inductive Step:
We have to show if the implementation is correct for a formula of complexity n, 
that it is also correct for one with complexity n+1.
We have multiple options for the highest order operation in a formula, f, of complexity n+1.
We will treat these separately:
1. sub (Neg f)
In this case the implementation produces: unionSet (Set [Neg f]) (sub f).
We can see from the implementation of unionSet, that it merges the two sets in order, 
while not allowing duplicates. The only subformulae that come from Neg f, are Neg f, and any subformulae from f.
Using our IA, we know that sub correctly creates a set of subformulae for f, which is a formula of complexity n.
Therefore, sub (Neg f) has the desired output and is correct.
2. sub (Cnj fs)
In this case the implementation produces: foldl unionSet (Set [f]) (map sub fs), where f = Cnj fs.
What this does, is it starts with the set of the full formula { Cnj fs },
and continuously merges in the set of subformulae for the next formula in fs.
As the conjunction is at the highest level, the subformulae consist of Cnj fs, 
as well as the sub formulae of each of the conjuncts. Using our IA, we know that (map sub fs) will correctly 
produce the set of subformulae for all at most nth-order elements of fs.
Therefore, this implementation is also correct.
3. sub (Dsj fs)
In this case the implementation produces: foldl unionSet (Set [f]) (map sub fs), where f = Dsj fs.
Identical reasoning as in (2.) is used to obtain that the implementation in this case is also correct.
4. sub (Impl f1 f2)
In this case the implementation produces: unionSet ( unionSet (Set [f]) (sub f1)) (sub f2), where f= Impl f1 f2.
The subformulae of the implication consist of the implication at large (Impl f1 f2),
as well as all the subformulae of f1 and f2.
the implementation merges the set of the large implication (f) with the subformulaes of f1 and f2, 
which using IA we assume to be complete and correct. Applying unionSet between a unionSet call and another set 
does not have any further implications, as it still simply merges the sets into each other while maintaining order.
5. sub (Equiv f1 f2)
In this case the implementation produces: unionSet ( unionSet (Set [f]) (sub f1)) (sub f2), where f= Impl f1 f2.
Identical reasoning as in (4.) is used to obtain that the implementation in this case is also correct.

These are all possible options for the top level operation of a formula of complexity n+1, thus we have shown that if the function
is correct for a complexity n, then it is also correct for formulae of complexity n+1.
in the base case, we have shown that it holds for n=0, therefore the function implementation is correct for all formulas
of any complexity.

2. I recursively implemented nsub based on the cases from sub, with nsub always equalling one plus the sum of all subformula subformulae.
The test property for the correct length uses a helper function to compute the number of elements in the set. We then compare whether the
number of subformulae is equivalent to the result of nsub, which turns out to be true for a chosen set of examples (Same as Exercise 7).


-}