module Exercise1 where

import LTS
import Data.List (intersect)
import Test.QuickCheck


prop_non_empty_Q :: IOLTS -> Bool
prop_non_empty_Q (states, inputs, outputs, transitions, initialState) = not (null states)

prop_disjoint_labels :: IOLTS -> Bool
prop_disjoint_labels (states, inputs, outputs, transitions, initialState) = null (intersect inputs outputs)

prop_T_correctness :: IOLTS -> Bool
prop_T_correctness (states, inputs, outputs, transitions, initialState) = all (\(f, l, t) -> f `elem` states && t `elem` states && (l `elem` inputs || l `elem` outputs)) transitions

prop_initial_state :: IOLTS -> Bool
prop_initial_state (states, inputs, outputs, transitions, initialState) = initialState `elem` states

validateLTS :: IOLTS -> Bool
validateLTS iolts = 
    prop_non_empty_Q iolts &&
    prop_disjoint_labels iolts &&
    prop_T_correctness iolts &&
    prop_initial_state iolts


--- Properties to test validateLTS
prop_test_empty_states :: IOLTS -> Bool
prop_test_empty_states (states, inputs, outputs, transitions, initialState) = null states || validateLTS (states, inputs, outputs, transitions, initialState)

prop_intersect_labels :: IOLTS -> Bool
prop_intersect_labels (states, inputs, outputs, transitions, initialState) = null (intersect inputs outputs) || not (validateLTS (states, inputs, outputs, transitions, initialState))

prop_tau_in_labels :: IOLTS -> Bool
prop_tau_in_labels (states, inputs, outputs, transitions, initialState) = not (any (\(_, l, _) -> l == tau) transitions) || not (validateLTS (states, inputs, outputs, transitions, initialState))

prop_initial_state_in_states :: IOLTS -> Bool
prop_initial_state_in_states (states, inputs, outputs, transitions, initialState) = initialState `elem` states || not (validateLTS (states, inputs, outputs, transitions, initialState))

exampleIOLTS :: IOLTS
exampleIOLTS = createIOLTS [(0, "?a", 1), (1, "!x", 2), (0, "?b", 3), (3, "!y", 4), (1, tau, 3), (4, tau, 2)]



{-
type State = Integer
type Label = String
type LabeledTransition = (State, Label, State)
type Trace = [Label]
type LTS = ([State], [Label], [LabeledTransition], State)
type IOLTS = ([State], [Label], [Label], [LabeledTransition], State)
-}

main :: IO ()
main = do
    putStrLn "Exercise 1"
    print $ validateLTS exampleIOLTS
