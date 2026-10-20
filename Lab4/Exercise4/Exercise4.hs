module Exercise4 where
import Data.List
import LTS
import Exercise2 (ltsGen)
import Exercise3 (tauClosure, isQuiescent, stepSet, straceLabels,
                  isStrace, ltsWithStraceGen, weakTracesUpTo)
import Test.QuickCheck

{-
p after sigma (Tretmans) is the set of states reachable from the initial state by
performing the observable trace sigma, where any number of tau steps may happen
before and after each visible label. delta is allowed in the trace: it keeps the
quiescent states of the current set (the delta self-loop of the suspension automaton).

Implementation: start with the tau-closure of the initial state and fold over the
trace with a step function. A visible label moves to all its successors and takes
their tau-closure again; delta keeps only the quiescent states. The result is a set
(sorted, no duplicates), so it can be compared with ==. The step functions are the
same as the ones used for straces in Exercise 3, so both definitions are consistent.
-}

infix 5 `after`

after :: IOLTS -> Trace -> [State]
lts `after` trace = makeSet (foldl step (tauClosure lts [initial]) trace)
  where
    (_, _, _, transitions, initial) = lts
    step current label
        | label == delta = filter (isQuiescent lts) current
        | otherwise = tauClosure lts [s' | (s, l, s') <- transitions, l == label, s `elem` current]

-- Tests

-- The empty trace leads to the tau-closure of the initial state
prop_AfterEmptyTrace :: Property
prop_AfterEmptyTrace = forAll ltsGen $ \lts@(_, _, _, _, initial) ->
    lts `after` [] == tauClosure lts [initial] && initial `elem` (lts `after` [])

-- Every trace (from the provided traces function, tau removed) leads to some state
prop_AfterTracesNonEmpty :: Property
prop_AfterTracesNonEmpty = forAll ltsGen $ \lts ->
    all (not . null . (lts `after`)) (weakTracesUpTo 4 lts)

-- Every strace leads to some state
prop_AfterStracesNonEmpty :: Property
prop_AfterStracesNonEmpty = forAll ltsWithStraceGen $ \(lts, trace) ->
    not (null (lts `after` trace))

-- A random sequence of labels is an strace iff after gives a non-empty set
prop_AfterEmptyIffNotStrace :: Property
prop_AfterEmptyIffNotStrace = forAll ltsGen $ \lts ->
    forAll (resize 4 (listOf (elements (straceLabels lts)))) $ \trace ->
        classify (isStrace lts trace) "is strace" $
        null (lts `after` trace) == not (isStrace lts trace)

-- after is compositional: after (s ++ r) is stepping r from after s
prop_AfterCompositional :: Property
prop_AfterCompositional = forAll ltsWithStraceGen $ \(lts, trace) ->
    forAll (choose (0, length trace)) $ \k ->
        let (sigma, rho) = splitAt k trace
        in lts `after` trace == makeSet (foldl (stepSet lts) (lts `after` sigma) rho)

-- after only returns states of the IOLTS
prop_AfterSubsetOfStates :: Property
prop_AfterSubsetOfStates = forAll ltsWithStraceGen $ \(lts@(states, _, _, _, _), trace) ->
    all (`elem` states) (lts `after` trace)

-- After a delta only quiescent states remain
prop_AfterDeltaQuiescent :: Property
prop_AfterDeltaQuiescent = forAll ltsWithStraceGen $ \(lts, trace) ->
    all (isQuiescent lts) (lts `after` (trace ++ [delta]))

-- Concrete examples from LTS.hs / Tretmans
prop_KnownAfter :: Bool
prop_KnownAfter =
    coffeeModel6 `after` ["coin"] == [2, 3] &&          -- tau step from 2 to 3
    coffeeModel6 `after` ["coin", "delta"] == [3] &&
    tretmanK3 `after` ["but"] == [1, 2] &&              -- non-determinism
    tretmanK1 `after` ["but", "liq"] == [2] &&
    tretmanK1 `after` ["delta"] == [0] &&
    tretmanK1 `after` ["but", "delta"] == [] &&          -- state 1 is not quiescent
    tretmanS4 `after` ["a"] == [1, 3] &&
    tretmanS4 `after` ["a", "x"] == [2]

main :: IO ()
main = do
    putStrLn "Known results from the provided models:"
    quickCheck (once prop_KnownAfter)
    putStrLn "after [] is the tau-closure of the initial state:"
    quickCheck prop_AfterEmptyTrace
    putStrLn "after every trace is non-empty:"
    quickCheck prop_AfterTracesNonEmpty
    putStrLn "after every strace is non-empty:"
    quickCheck prop_AfterStracesNonEmpty
    putStrLn "after is empty iff the label sequence is not an strace:"
    quickCheck prop_AfterEmptyIffNotStrace
    putStrLn "after is compositional:"
    quickCheck prop_AfterCompositional
    putStrLn "after only returns existing states:"
    quickCheck prop_AfterSubsetOfStates
    putStrLn "after a delta only quiescent states remain:"
    quickCheck prop_AfterDeltaQuiescent
