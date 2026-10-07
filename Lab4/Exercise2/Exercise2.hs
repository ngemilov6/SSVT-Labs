module Exercise2 where

import LTS
import Exercise1 (validateLTS,prop_test_empty_states, prop_intersect_labels, prop_tau_in_labels, prop_initial_state_in_states)
import Test.QuickCheck


genLabel :: Gen Label
genLabel = do
    n     <- chooseInt (1, 10)
    label <- vectorOf n (elements (['a'..'z'] ++ ['A'..'Z']))
    pure label

ioltsGen :: Gen IOLTS
ioltsGen = do 
    states <- listOf1 arbitrary :: Gen [State]
    inputs' <- listOf genLabel
    outputs <- listOf genLabel
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
    quickCheck $ forAll ioltsGen validateLTS
    quickCheck $ forAll ioltsGen prop_test_empty_states
    quickCheck $ forAll ioltsGen prop_intersect_labels
    quickCheck $ forAll ioltsGen prop_tau_in_labels
    quickCheck $ forAll ioltsGen prop_initial_state_in_states