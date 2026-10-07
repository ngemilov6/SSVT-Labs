module Exercise5 where

import LTS (delta, tau, IOLTS, LTS, Trace, State, Label, LabeledTransition, traces, doorImpl1, doorImpl2, doorImpl3, doorImpl4, doorImpl5, doorImpl6, doorImpl7, doorImpl8, createLTS)
import Exercise1 (exampleIOLTS, validateLTS,prop_test_empty_states, prop_intersect_labels, prop_tau_in_labels, prop_initial_state_in_states)
import Exercise2 (ioltsGen)
import Exercise3 (find_quiescent_states, straces, generate_transitions_straces)
import Exercise4 (after)
import Test.QuickCheck
import Data.List (nub)

testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT (states, labels, transitions, initialState) sutFunction =
    all (\(f, l, t) -> let (sutState, sutLabel) = sutFunction f l in sutState == t) transitions


doorLTS1 :: LTS
doorLTS1 = createLTS [(0, "close", 1), (1, "open", 0), (1, "lock", 2), (2, "unlock", 1)]

main :: IO ()
main = do
    putStrLn "Exercise 5"
    print (testLTSAgainstSUT doorLTS1 doorImpl1)
    print (testLTSAgainstSUT doorLTS1 doorImpl2)
    print (testLTSAgainstSUT doorLTS1 doorImpl3)
    -- print (testLTSAgainstSUT doorLTS1 doorImpl4)
    print (testLTSAgainstSUT doorLTS1 doorImpl5)
    print (testLTSAgainstSUT doorLTS1 doorImpl6)
    print (testLTSAgainstSUT doorLTS1 doorImpl7)
    print (testLTSAgainstSUT doorLTS1 doorImpl8)