module Exercise2 where

import LTS
import Exercise1 (validateLTS,prop_test_empty_states, prop_intersect_labels, prop_tau_in_labels, prop_initial_state_in_states)
import Test.QuickCheck

ltsGen :: Gen IOLTS
ltsGen = do 
    states <- listOf1 arbitrary
    inputs' <- listOf arbitrary
    outputs <- listOf arbitrary
    let inputs = filter (`notElem` outputs) inputs'
    transitions <- if null inputs then return [] else listOf $ do
        f <- elements states
        l <- elements (inputs ++ outputs)
        t <- elements states
        return (f, l, t)
    initialState <- elements states
    return (states, inputs, outputs, transitions, initialState)


main :: IO ()
main = do
    putStrLn "Exercise 2"
    quickCheck $ forAll ltsGen validateLTS
    quickCheck $ forAll ltsGen prop_test_empty_states
    quickCheck $ forAll ltsGen prop_intersect_labels
    quickCheck $ forAll ltsGen prop_tau_in_labels
    quickCheck $ forAll ltsGen prop_initial_state_in_states