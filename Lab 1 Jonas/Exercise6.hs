{-# LANGUAGE NoExplicitForAll #-}

module Exercise6 where

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

-- From task
data Boy = Matthew | Peter | Jack | Arnold | Carl deriving (Eq,Show)
data PropType = Truth | Thief deriving (Eq,Show)
boys = [Matthew, Peter, Jack, Arnold, Carl]

type Name = (Boy, PropType)
{-
I split the propositions into two types:
 1. Whether a boy is telling the truth or not
 2. Whether a boy is the thief or not
 This allows for full representation of the statements made by the boys.
-}

-- Data type for formula and instance of Show from lecture, including showLst and showRest functions.
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


-- Manual translation of the statements using propositional logic.

statementMatthew :: Form --Matthew: Carl didn't do it, and neither did I.
statementMatthew = Cnj[Neg (Prop (Carl, Thief)), Neg (Prop (Matthew, Thief))]
ruleMatthew :: Form
ruleMatthew = Equiv (Prop (Matthew, Truth)) statementMatthew

statementPeter :: Form --Peter: It was Matthew or it was Jack.
statementPeter = Dsj [Prop (Matthew, Thief), Prop (Jack, Thief)]
rulePeter :: Form
rulePeter = Equiv (Prop (Peter, Truth)) statementPeter

statementJack :: Form --Jack: Matthew and Peter are both lying.
statementJack = Cnj [Neg (Prop (Matthew, Truth)), Neg (Prop (Peter, Truth))] 
ruleJack :: Form
ruleJack = Equiv (Prop (Jack, Truth)) statementJack

statementArnold :: Form --Arnold: Matthew or Peter is speaking the truth, but not both.
statementArnold = Equiv (Prop (Matthew, Truth)) (Neg (Prop (Peter, Truth)))
ruleArnold :: Form
ruleArnold = Equiv (Prop (Arnold, Truth)) statementArnold

statementCarl :: Form --Carl: What Arnold says is not true.
statementCarl = Neg (Prop (Arnold, Truth))
ruleCarl :: Form
ruleCarl = Equiv (Prop (Carl, Truth)) statementCarl

-- Generation of possible combinations of truth-telling
-- This allows us to restrict valuations according to the teacher's restriction that 3 always tell the truth and 2 always lie.
combinations :: Int -> [Boy] -> [[Boy]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs

-- From this we can also generate a formula that represents this valuation.
formVal :: Int -> [Boy] -> Form
formVal n people =
    Dsj[Cnj [ if person `elem` truthfulPeople then Prop (person, Truth) else Neg (Prop (person, Truth)) 
            | person <- people]
        | truthfulPeople <- combinations n people]

--- Additional rule that there can only be one Thief, based on the above definition.
oneThief :: Form
oneThief = Dsj[Cnj [ if person `elem` thiefPerson then Prop (person, Thief) else Neg (Prop (person, Thief)) 
            | person <- boys]
        | thiefPerson <- combinations 1 boys]

--- Definition of the predicate to check representing the crime scene investigation
csi :: Form
csi = Cnj [
    ruleMatthew,
    rulePeter,
    ruleJack,
    ruleArnold,
    ruleCarl,
    formVal 3 boys,
    oneThief]

--- Valuation Type
type Valuation = [(Name, Bool)]

--- Function to evaluate a formula under a valuation
eval :: Valuation -> Form -> Bool
eval valuation (Prop (name, ptype)) = case lookup (name, ptype) valuation of
    Just truthValue -> truthValue
    Nothing -> error ("No valuation found for: " ++ show name)
eval valuation (Neg form) = not (eval valuation form)
eval valuation (Cnj forms) = all (eval valuation) forms
eval valuation (Dsj forms) = any (eval valuation) forms
eval valuation (Impl left right) = not (eval valuation left) || eval valuation right
eval valuation (Equiv left right) = eval valuation left == eval valuation right

--- Generate all possible valuations, by zipping the names to all possible truth assignments generated using sequence.
allValuations :: [Name] -> [Valuation]
allValuations names = map (zip names) truthAssignments where 
    truthAssignments = sequence (replicate (length names) [False, True])

--- Find the solutions as all valuations that evaluate to being True, out of the set of possible valuations for both Thief and Truth types.
solutions :: Form -> [Valuation]
solutions form = filter (\valuation -> eval valuation form) (allValuations [(boy, ptype) | boy <- boys, ptype <- [Truth, Thief]])


--- Functions to extract the person who is guilty as the thief and the honest people that were telling the truth.
guilty, honest :: [Valuation] -> [Boy]
guilty valuation = [boy | boy <- boys, valuation1 <- valuation, ((boy, Thief), True) `elem` valuation1]
honest valuation = [boy | boy <- boys, valuation1 <- valuation, ((boy, Truth), True) `elem` valuation1]




{- Did not use these as the labels of accuses and accusers was confusing in thinking about the problem
findHonest :: (Eq a) => [a] -> [a] -> Maybe a
findHonest = undefined

findThief :: (Eq a) => [a] -> [a] -> Maybe a
findThief = undefined

accuses :: Boy -> Boy -> Bool
accuses = undefined

accusers :: Boy -> [Boy]
accusers = undefined
-}

main :: IO()
main = do 
    let solution = solutions csi
    let lengthS = length solution
    -- print (show solution)
    --- Distinguish the output by whether there is a unique solution or not.
    if lengthS == 1 then do print ("The person that is guilty is: " ++ show (guilty solution)) 
                            print ("The people that were honest are: " ++ show (honest solution))
                    else
                        print "There is more than one solution."
    


{- Time taken: 3h
Additional Notes:
As the focus of the exercise was the formulation of requirements, I did not spend a large amount of time on the optimization
of the selection of valuations, but for this simply used the list of all valuations. Considering the rather small number of variables that
is also not too computationally expensive. Optimizations here can be done to not have to iterate over all valuations when finding the solution.
-}