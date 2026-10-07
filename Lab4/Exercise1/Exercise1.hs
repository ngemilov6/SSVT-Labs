module Exercise1 where

import LTS


prop_countable_non_empty_Q :: IOLTS -> Bool
prop_countable_non_empty_Q (states, inputs, outputs, transitions, initialState) = length states <= length [0..] && not (null states)

prop_countable_labels :: IOLTS -> Bool
prop_countable_labels (states, inputs, outputs, transitions, initialState) = length inputs <= length [0..] && length outputs <= length [0..]

prop_disjoint_labels :: IOLTS -> Bool
prop_disjoint_labels (states, inputs, outputs, transitions, initialState) = null (intersect inputs outputs)

prop_T_correctness :: IOLTS -> Bool
prop_T_correctness (states, inputs, outputs, transitions, initialState) = all (\(f, l, t) -> f `elem` states && t `elem` states && (l `elem` inputs || l `elem` outputs)) transitions

prop_initial_state :: IOLTS -> Bool
prop_initial_state (states, inputs, outputs, transitions, initialState) = initialState `elem` states

validateLTS :: IOLTS -> Bool
validateLTS iolts = 
    prop_countable_non_empty_Q iolts &&
    prop_countable_labels iolts &&
    prop_disjoint_labels iolts &&
    prop_T_correctness iolts &&
    prop_initial_state iolts



main :: IO ()
main = do
    putStrLn "Exercise 1"