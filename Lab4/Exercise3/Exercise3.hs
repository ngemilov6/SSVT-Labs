module Exercise3 where

import LTS (delta, IOLTS, Trace, State, LabeledTransition, traces)
import Exercise1 (exampleIOLTS, validateLTS,prop_test_empty_states, prop_intersect_labels, prop_tau_in_labels, prop_initial_state_in_states)
import Exercise2 (ioltsGen)
import Test.QuickCheck



find_quiescent_states :: IOLTS -> [State]
find_quiescent_states (states, inputs, outputs, transitions, initialState) =
    filter isQuiescent states
  where
    isQuiescent state = null [l | (f, l, t) <- transitions, f == state, l `elem` outputs]

generate_transitions_straces :: IOLTS -> [LabeledTransition]
generate_transitions_straces (states, inputs, outputs, transitions, initialState) =
    let quiescentStates = find_quiescent_states (states, inputs, outputs, transitions, initialState)
        deltaRelations = [(s, delta, s) | s <- quiescentStates]
    in deltaRelations ++ transitions

straces :: IOLTS -> [Trace]
straces iolts@(states, inputs, outputs, transitions, initialState) =
    let extendedTransitions = generate_transitions_straces iolts
    in traces (states, inputs ++ outputs, extendedTransitions, initialState)

genRandomStraces :: Gen [Trace]
genRandomStraces = do
    iolts <- ioltsGen
    return (straces iolts)

main :: IO ()
main = do
    putStrLn "Exercise 3"
    print (take 20 $ straces exampleIOLTS)
    traces <- take 20 <$> generate genRandomStraces
    print traces