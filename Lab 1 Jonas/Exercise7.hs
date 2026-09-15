{-# LANGUAGE NoExplicitForAll #-}

module Exercise7 where

import Data.List
import System.Random
import Test.QuickCheck
-- import Lecture1
-- import Lecture2
-- import Lecture3


infix 1 -->
(-->) :: Bool -> Bool -> Bool
p --> q = (not p) || q

-- forall :: [a] -> (a -> Bool) -> Bool
-- forall = flip all


--- Definitions from Lecture 3
type Name = Int


data Form
  = Prop Name
  | Neg Form
  | Cnj [Form]
  | Dsj [Form]
  | Impl Form Form
  | Equiv Form Form
  deriving (Eq)

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

nnf :: Form -> Form
nnf (Prop x) = Prop x
nnf (Neg (Prop x)) = Neg (Prop x)
nnf (Neg (Neg f)) = nnf f
nnf (Cnj fs) = Cnj (map nnf fs)
nnf (Dsj fs) = Dsj (map nnf fs)
nnf (Neg (Cnj fs)) = Dsj (map (nnf . Neg) fs)
nnf (Neg (Dsj fs)) = Cnj (map (nnf . Neg) fs)
---
--- ^^^^^ Definitions from Lecture 3

--- No implication form definition, to remove implications
nif :: Form -> Form
nif (Prop x) = Prop x
nif (Neg x) = Neg (nif x)
nif (Cnj xs) = Cnj (map nif xs)
nif (Dsj xs) = Dsj (map nif xs)
nif (Impl left right) = Dsj [Neg (nif left), nif right]
nif (Equiv left right) = Dsj [Cnj [nif left, nif right], Cnj [Neg (nif left), Neg (nif left)]]

cnf :: Form -> Form
cnf form = cleanCNF ninf where ninf = nnf (nif form)

--- Merger to combine multiple conjunctions on the same level.
mergeCnj :: [Form] -> [Form]
mergeCnj [] = []
mergeCnj (x:xs) = case x of
  Cnj forms -> forms ++ mergeCnj xs
  form -> [form] ++ mergeCnj xs

--- Computes the permutations of the elements in the disjunction, especially conjunctions, to form the top-level conjunction.
--- This also includes cases at the end to merge multiple disjunctions on the same level into one.
distributeOr :: Form -> Form -> Form
distributeOr (Cnj left) (Cnj right) = Cnj [distributeOr l r | l<- left, r<-right]
distributeOr (Cnj left) right = Cnj [distributeOr l right | l <- left]
distributeOr left (Cnj right) = Cnj [distributeOr left r| r<-right]
distributeOr left right = case (left, right) of 
  (Dsj formsl, Dsj formsr) -> Dsj (formsl ++ formsr)
  (Dsj formsl, formr) -> Dsj (formsl ++ [formr])
  (forml, Dsj formsr) -> Dsj (forml:formsr)
  (forml, formr) -> Dsj [forml, formr]

--- Cleaned CNF
cleanCNF :: Form -> Form
cleanCNF (Prop x) = Prop x
cleanCNF (Neg (Prop x)) = Neg (Prop x)
cleanCNF (Cnj xs) = Cnj (mergeCnj (map cleanCNF xs))
cleanCNF (Dsj xs) = foldr1 distributeOr (map cleanCNF xs)

--- From Lecture 3
---
---
propNames :: Form -> [Name]
propNames = sort . nub . pnames
  where
    pnames (Prop name) = [name]
    pnames (Neg f) = pnames f
    pnames (Cnj fs) = concatMap pnames fs
    pnames (Dsj fs) = concatMap pnames fs
    pnames (Impl f1 f2) = concatMap pnames [f1, f2]
    pnames (Equiv f1 f2) = concatMap pnames [f1, f2]

type Valuation = [(Name, Bool)]

--- Generate a random number, adjusted from previous exercises and lecture material
genSmallNat :: Int -> Int -> Gen Int
genSmallNat a b = chooseInt (a, b)

-- | all possible valuations for lists of prop letters
genVals :: [Name] -> [Valuation]
genVals [] = [[]]
genVals (name : names) =
  map ((name, True) :) (genVals names)
    ++ map ((name, False) :) (genVals names)

genRanVal :: [Valuation] -> Gen Valuation
genRanVal valuations = do
  k <- genSmallNat 0 ((length valuations)-1)
  return (valuations !! k)


evl :: Valuation -> Form -> Bool
evl [] (Prop c) = error ("no info: " ++ show c)
evl ((i, b) : xs) (Prop c)
  | c == i = b
  | otherwise = evl xs (Prop c)
evl xs (Neg f) = not (evl xs f)
evl xs (Cnj fs) = all (evl xs) fs
evl xs (Dsj fs) = any (evl xs) fs
evl xs (Impl f1 f2) = evl xs f1 --> evl xs f2
evl xs (Equiv f1 f2) = evl xs f1 == evl xs f2

checkEval :: Form -> Form -> Gen Bool
checkEval form cnf = do
  valuation <- genRanVal (genVals (propNames form))
  return (evl valuation form == evl valuation cnf)

checks :: [Form] -> Gen [Bool]
checks examples = mapM (\ex -> checkEval ex (cnf ex)) examples

--- ^From Lecture 3
---
---
main :: IO()
main = do 
    let example = (Equiv (Impl (Prop 1) (Prop 2)) (Dsj [Neg (Prop 3), Prop 4]))
    let example2 = (Dsj [Cnj [Prop 1, Prop 2], Cnj [Neg (Prop 3), Prop 4]])
    let example3 = (Dsj [Prop 1, Cnj [Prop 2, Dsj [Neg (Prop 3), Cnj[Prop 4, Prop 5]]]])
    let example4 = (Dsj [Dsj [Prop 1, Cnj [Prop 2, Neg (Prop 3)]], Cnj[Prop 4, Prop 5]])
    let example5 = (Dsj [Cnj[Prop 1, Neg (Prop 2)], Cnj[Neg (Prop 3), Prop 4], Cnj[Prop 5, Prop 6]])
    print (cnf example)
    print (cnf example2)
    print (cnf example3)
    print (cnf example4)
    print (cnf example5)
    checkList <- generate (checks [example, example2, example3, example4, example5])
    print checkList
    


{- Time taken: 120min
I started with the formula definitions and NNF definition from Lecture 3.
I then defined a function for eliminating implications and bi-implications, NIF, using the known logical equivalences.
For the CNF I first apply nif and then nnf to have a formula without implications where I can see Neg( Prop ) | Prop as the smalles unit.
To obtain CNF I recursively process the formula, finding the CNF of subformulas of conjunctions and merging conjunctions on the same level.
For disjunctions I distribute the disjunction through all its terms recursively, for which I created distributeOr. 
I use foldr1 to only need to go through the list once and trigger follow-up recursions where necessary.
In distributeOr I also use a case analysis to merge discunctions on the same level, 
to keep the complexity minimal and reduce the formula as far as possible.
For testing I used generators from previous exercises and Lecture 3 to generate valuations, pick a random one, 
and evaluate a formula and its CNF to check if the result is the same.
For this I compiled a list of five examples that aim to test different cases of implications, conjunctions on equal level,
stacked conjunction/disjunctions, single properties in conjunction/disjunction with more conjunctions/disjunctions.
-}