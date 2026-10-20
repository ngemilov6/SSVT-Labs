module Exercise2 where
import Data.List
import LTS
import Exercise1 hiding (main)
import Test.QuickCheck

-- Label pools. Following createIOLTS, labels are stored without '?' / '!' prefix.
-- The pools are disjoint, so inputs and outputs never overlap by construction.
inputPool, outputPool :: [Label]
inputPool = ["a", "b", "c"]
outputPool = ["x", "y", "z"]

-- Random generator for valid IOLTS's.
-- States are [0 .. n-1] with 0 as initial state (the createIOLTS convention).
-- Every state k > 0 gets one incoming transition from a lower state, so all states
-- are reachable from the initial state; extra random transitions add branching,
-- self-loops and cycles. The number of states grows with the QuickCheck size.
ltsGen :: Gen IOLTS
ltsGen = sized $ \size -> do
    numStates <- choose (1, 2 + toInteger size `div` 15)
    let states = [0 .. numStates - 1]
    inputs <- sublistOf inputPool
    outputs <- sublistOf outputPool
    let labelGen = labelFor (inputs ++ outputs)
    spanning <- mapM (\k -> do
                         from <- choose (0, k - 1)
                         l <- labelGen
                         return (from, l, k)) (drop 1 states)
    numExtra <- choose (0, 2 * fromInteger numStates)
    extra <- vectorOf numExtra $ do
        s <- elements states
        l <- labelGen
        s' <- elements states
        return (s, l, s')
    return (states, inputs, outputs, makeSet (spanning ++ extra), 0)

-- tau is possible but less frequent than visible labels
labelFor :: [Label] -> Gen Label
labelFor [] = return tau
labelFor visible = frequency [(1, return tau), (6, elements visible)]

-- Generator for invalid IOLTS's: a valid IOLTS with one invalidating factor injected
invalidLtsGen :: Gen IOLTS
invalidLtsGen = do
    lts <- ltsGen
    (_, mutation) <- elements invalidatingMutations
    return (mutation lts)

-- Additional generator: valid IOLTS's without internal (tau) transitions
tauFreeLtsGen :: Gen IOLTS
tauFreeLtsGen = do
    (q, i, o, t, q0) <- ltsGen
    return (q, i, o, filter (\(_, l, _) -> l /= tau) t, q0)

-- Additional generator: input-enabled IOLTS's (IOTS in Tretmans), every state accepts every input
inputEnabledLtsGen :: Gen IOLTS
inputEnabledLtsGen = do
    (q, i, o, t, q0) <- ltsGen
    let missing = [(s, l, s) | s <- q, l <- i, null [() | (s1, l1, _) <- t, s1 == s, l1 == l]]
    return (q, i, o, makeSet (t ++ missing), q0)

-- Characteristics of generated IOLTS's, used to show the distribution
hasTau, hasCycle :: IOLTS -> Bool
hasTau (_, _, _, t, _) = any (\(_, l, _) -> l == tau) t
hasCycle (_, _, _, t, _) = any (\(s, _, s') -> s' <= s) t

-- QuickCheck tests for validateLTS using the generators

prop_LtsGenValid :: Property
prop_LtsGenValid = forAll ltsGen $ \lts@(q, _, _, _, _) ->
    classify (hasTau lts) "has tau" $
    classify (hasCycle lts) "has cycle or self-loop" $
    classify (length q > 3) "more than 3 states" $
    validateLTS lts

prop_InvalidLtsGenInvalid :: Property
prop_InvalidLtsGenInvalid = forAll invalidLtsGen (not . validateLTS)

-- Each mutation separately, labelled, to show that every factor is detected
prop_EveryFactorDetected :: Property
prop_EveryFactorDetected = forAll ltsGen $ \lts ->
    forAll (choose (0, length invalidatingMutations - 1)) $ \k ->
        let (name, mutation) = invalidatingMutations !! k
        in label name $ not (validateLTS (mutation lts))

prop_OtherGeneratorsValid :: Property
prop_OtherGeneratorsValid = forAll (oneof [tauFreeLtsGen, inputEnabledLtsGen]) validateLTS

main :: IO ()
main = do
    putStrLn "ltsGen only produces valid IOLTS's:"
    quickCheck prop_LtsGenValid
    putStrLn "invalidLtsGen only produces invalid IOLTS's:"
    quickCheck prop_InvalidLtsGenInvalid
    putStrLn "Every invalidating factor is detected:"
    quickCheck prop_EveryFactorDetected
    putStrLn "tauFreeLtsGen and inputEnabledLtsGen produce valid IOLTS's:"
    quickCheck prop_OtherGeneratorsValid
    putStrLn "\n--- Exercise 1 properties on random valid IOLTS's ---"
    mapM_ (\(name, p) -> putStrLn (name ++ ":") >> quickCheck (forAll ltsGen p)) validProperties
