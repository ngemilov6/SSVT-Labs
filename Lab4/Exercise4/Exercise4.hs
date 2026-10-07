module Exercise4 where

import LTS (delta, tau, IOLTS, Trace, State, Label, LabeledTransition, traces)
import Exercise1 (exampleIOLTS, validateLTS,prop_test_empty_states, prop_intersect_labels, prop_tau_in_labels, prop_initial_state_in_states)
import Exercise2 (ioltsGen)
import Exercise3 (find_quiescent_states, straces, generate_transitions_straces)
import Test.QuickCheck
import Data.List (nub)


step :: [LabeledTransition] -> [State] -> Label -> [State]
step transitions currentStates label =
    close [s | (f, l, s) <- transitions, f `elem` currentStates, l == label]
    where
        close states =
            let next = nub (states ++ [s | (f, l, s) <- transitions, f `elem` states, l == tau])
            in if next == states then states else close next

after :: IOLTS -> Trace -> [State]
after (states, inputs, outputs, transitions, initialState) trace =
    let extendedTransitions = generate_transitions_straces (states, inputs, outputs, transitions, initialState)
        reachableStates = foldl (step extendedTransitions) [initialState] trace
    in reachableStates
        

main :: IO ()
main = do
    putStrLn "Exercise 4"
    print (after exampleIOLTS ["a", "y"])