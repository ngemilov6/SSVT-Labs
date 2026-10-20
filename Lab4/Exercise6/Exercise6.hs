module Exercise6 where
import Data.List
import LTS
import Exercise2 (ltsGen)
import Exercise3 (isQuiescent)
import Test.QuickCheck

{-
The visualization is plain text for the terminal. The transitions are grouped per
source state, with the labels aligned in one column:

    -> 1  ?coin   -> 2
          delta   -> 1
       2  !coffee -> 3
          tau     -> 3
       3  delta   -> 3

"->" in front of a state marks the initial state. Every state is shown, also states
without outgoing transitions.

visualizeLTS shows the labels of an LTS as they are. visualizeIOLTS also prints
the inputs and outputs. IOLTS labels are stored without prefix, so it adds '?' to
inputs and '!' to outputs. Quiescent states get a delta self-loop, which shows the
suspension automaton used for straces.

visualizeRandomLTS generates an IOLTS with ltsGen (Exercise 2) and prints it.
-}

-- Renders every state with its outgoing transitions, labels aligned in one column
render :: [State] -> State -> [LabeledTransition] -> String
render states initial transitions = unlines (concatMap stateLines (makeSet (initial : states)))
  where
    stateWidth = maximum (map (length . show) (initial : states))
    labelWidth = maximum (0 : [length l | (_, l, _) <- transitions])
    pad n str = str ++ replicate (n - length str) ' '

    stateLines s =
        let marker = if s == initial then "-> " else "   "
            name = pad stateWidth (show s)
            blank = replicate (3 + stateWidth) ' '
        in case [(l, s') | (from, l, s') <- transitions, from == s] of
            [] -> [marker ++ name ++ "  (no outgoing transitions)"]
            (first : rest) -> (marker ++ name ++ edge first) : map ((blank ++) . edge) rest

    edge (l, s') = "  " ++ pad labelWidth l ++ " -> " ++ show s'

visualizeLTS :: LTS -> String
visualizeLTS (states, labels, transitions, initial) =
    "Labels: " ++ unwords labels ++ "\n" ++ render states initial transitions

visualizeIOLTS :: IOLTS -> String
visualizeIOLTS lts@(states, inputs, outputs, transitions, initial) =
    "Inputs: " ++ unwords (map ('?' :) inputs) ++ "\n" ++
    "Outputs: " ++ unwords (map ('!' :) outputs) ++ "\n" ++
    render states initial (makeSet ([(s, showLabel l, s') | (s, l, s') <- transitions] ++ deltaLoops))
  where
    showLabel l | l `elem` inputs = '?' : l
                | l `elem` outputs = '!' : l
                | otherwise = l
    deltaLoops = [(s, delta, s) | s <- states, isQuiescent lts s]

-- Visualize a random IOLTS created with the generator from Exercise 2
visualizeRandomLTS :: IO ()
visualizeRandomLTS = do
    lts <- generate ltsGen
    putStrLn $ visualizeIOLTS lts

main :: IO ()
main = do
    putStrLn "Provided LTS tretmanV (with a tau transition):"
    putStrLn $ visualizeLTS tretmanV
    putStrLn "Provided IOLTS coffeeModel6:"
    putStrLn $ visualizeIOLTS coffeeModel6
    putStrLn "Random IOLTS from ltsGen:"
    visualizeRandomLTS
