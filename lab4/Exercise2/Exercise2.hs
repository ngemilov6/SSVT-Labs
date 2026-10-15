-- Additional dependencies: none
-- Time Spent: X min
module Exercise2 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise1

genTransition :: [State] -> [Label] -> Gen LabeledTransition
genTransition states labels = do
    from  <- elements states
    label <- elements labels
    to    <- elements states
    return (from, label, to)

ltsGen :: Gen IOLTS
ltsGen = do
    n <- choose (1,5)
    k <- choose (1,5)
    let states = [1..n]

    let pool = ["a", "b", "c", "d", "e"]
    labels <- sublistOf pool `suchThat` (not . null)
    let inputs = filter (`elem` labels) pool
    let outputs = filter (`notElem` inputs) pool
    transitions <- vectorOf k (genTransition states (pool  ++ [tau]))
    return (states, inputs, outputs, transitions,1)