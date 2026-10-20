module Exercise3 where
import Data.List
import LTS
import Exercise2 (ltsGen)
import Test.QuickCheck

{-
A suspension trace (Tretmans) is a trace over L_I u L_U u {delta} of the system in
which quiescent states get an extra delta self-loop. A state is quiescent if it can
not do any output and no tau step. tau itself is unobservable, so it never appears
in a trace: after each visible label we take the tau-closure of the reached states.

straces works on SETS of states instead of single paths (subset construction):
it starts from the tau-closure of the initial state and for every label computes
the successor set. A trace is emitted when its set is non-empty. The traces are
produced level by level (breadth first), so every strace occurs exactly once and in
order of length. The list is infinite as soon as a quiescent state is reachable
(delta can be repeated), therefore consumers use takeWhile on the length.
-}

-- States reachable with zero or more tau transitions
tauClosure :: IOLTS -> [State] -> [State]
tauClosure (_, _, _, transitions, _) = go . makeSet
  where
    go current =
        let new = makeSet [s' | (s, l, s') <- transitions, l == tau, s `elem` current, s' `notElem` current]
        in if null new then current else go (makeSet (current ++ new))

-- A state is quiescent if it has no outgoing output and no outgoing tau transition
isQuiescent :: IOLTS -> State -> Bool
isQuiescent (_, _, outputs, transitions, _) s =
    null [l | (from, l, _) <- transitions, from == s, l == tau || l `elem` outputs]

-- One observable step from a (tau-closed) set of states
stepSet :: IOLTS -> [State] -> Label -> [State]
stepSet lts@(_, _, _, transitions, _) current label
    | label == delta = filter (isQuiescent lts) current
    | otherwise = tauClosure lts [s' | (s, l, s') <- transitions, l == label, s `elem` current]

initialSet :: IOLTS -> [State]
initialSet lts@(_, _, _, _, initial) = tauClosure lts [initial]

-- Labels that can occur in a suspension trace
straceLabels :: IOLTS -> [Label]
straceLabels (_, inputs, outputs, _, _) = makeSet (inputs ++ outputs) ++ [delta]

straces :: IOLTS -> [Trace]
straces lts = levels [([], initialSet lts)]
  where
    levels [] = []
    levels level = map fst level ++ levels (concatMap expand level)
    expand (trace, current) =
        [ (trace ++ [l], next) | l <- straceLabels lts
                               , let next = stepSet lts current l
                               , not (null next) ]

-- All straces up to a given length (finite, because straces is ordered by length)
stracesUpTo :: Int -> IOLTS -> [Trace]
stracesUpTo n = takeWhile ((<= n) . length) . straces

isStrace :: IOLTS -> Trace -> Bool
isStrace lts trace = trace `elem` stracesUpTo (length trace) lts

-- Random suspension trace: a random walk over enabled labels (delta included)
straceGen :: IOLTS -> Gen Trace
straceGen lts = do
    len <- choose (0, 5 :: Int)
    walk len (initialSet lts) []
  where
    walk 0 _ trace = return trace
    walk k current trace =
        case [(l, next) | l <- straceLabels lts, let next = stepSet lts current l, not (null next)] of
            [] -> return trace
            enabled -> do
                (l, next) <- elements enabled
                walk (k - 1) next (trace ++ [l])

-- Random IOLTS together with one of its suspension traces
ltsWithStraceGen :: Gen (IOLTS, Trace)
ltsWithStraceGen = do
    lts <- ltsGen
    trace <- straceGen lts
    return (lts, trace)

-- Weak traces from the provided traces function (tau removed), bounded in length
weakTracesUpTo :: Int -> IOLTS -> [Trace]
weakTracesUpTo n (q, i, o, t, q0) =
    nub $ map (filter (/= tau)) $ takeWhile ((<= n) . length) $ traces (q, i ++ o, t, q0)

-- Properties

-- tau is not observable, so it never occurs in an strace
prop_NoTauInStraces :: Property
prop_NoTauInStraces = forAll ltsGen $ \lts -> all (notElem tau) (stracesUpTo 4 lts)

-- Every prefix of an strace is an strace
prop_PrefixClosed :: Property
prop_PrefixClosed = forAll ltsWithStraceGen $ \(lts, trace) ->
    all (isStrace lts) (inits trace)

-- Every (weak) trace of the IOLTS is also an strace
prop_TracesAreStraces :: Property
prop_TracesAreStraces = forAll ltsGen $ \lts ->
    all (isStrace lts) (weakTracesUpTo 4 lts)

-- delta only removes states, so leaving out deltas still gives an strace
prop_DeltaRemovalPreserves :: Property
prop_DeltaRemovalPreserves = forAll ltsWithStraceGen $ \(lts, trace) ->
    isStrace lts (filter (/= delta) trace)

-- Quiescence is persistent: after a delta, delta can always be repeated
prop_DeltaRepeatable :: Property
prop_DeltaRepeatable = forAll ltsWithStraceGen $ \(lts, trace) ->
    let quiescentAfter = isStrace lts (trace ++ [delta])
    in classify quiescentAfter "delta possible" $
       not quiescentAfter || isStrace lts (trace ++ [delta, delta])

-- delta can follow a trace iff a quiescent state is reachable after that trace
prop_DeltaIffQuiescent :: Property
prop_DeltaIffQuiescent = forAll ltsWithStraceGen $ \(lts, trace) ->
    let reached = foldl (stepSet lts) (initialSet lts) trace
    in isStrace lts (trace ++ [delta]) == any (isQuiescent lts) reached

-- The traces generator only produces straces
prop_GeneratedAreStraces :: Property
prop_GeneratedAreStraces = forAll ltsWithStraceGen $ \(lts, trace) ->
    collect (length trace) $ isStrace lts trace

-- Concrete examples from LTS.hs / Tretmans
prop_KnownStraces :: Bool
prop_KnownStraces =
    isStrace coffeeModel6 ["coin", "delta"] &&          -- quiescent state 3 reached via tau
    isStrace coffeeModel6 ["coin", "coffee", "delta"] &&
    not (isStrace coffeeModel6 ["delta", "coffee"]) &&  -- no output before the coin
    isStrace tretmanS4 ["a", "delta"] &&                -- tau to quiescent state 3
    isStrace tretmanS4 ["a", "x", "delta"] &&
    isStrace tretmanK1 ["delta", "but", "liq", "delta"] &&
    not (isStrace tretmanK1 ["but", "delta"])           -- state 1 must produce liq

main :: IO ()
main = do
    putStrLn "Known straces of the provided models:"
    quickCheck (once prop_KnownStraces)
    putStrLn "straces never contain tau:"
    quickCheck prop_NoTauInStraces
    putStrLn "straces are prefix closed:"
    quickCheck prop_PrefixClosed
    putStrLn "Every trace (tau removed) is an strace:"
    quickCheck prop_TracesAreStraces
    putStrLn "Removing deltas from an strace gives an strace:"
    quickCheck prop_DeltaRemovalPreserves
    putStrLn "delta can be repeated:"
    quickCheck prop_DeltaRepeatable
    putStrLn "delta is possible iff a quiescent state is reached:"
    quickCheck prop_DeltaIffQuiescent
    putStrLn "The random traces generator only produces straces (lengths shown):"
    quickCheck prop_GeneratedAreStraces
