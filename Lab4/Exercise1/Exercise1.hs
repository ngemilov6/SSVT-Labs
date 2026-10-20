module Exercise1 where
import Data.List
import LTS
import Test.QuickCheck

{-
Factors that result in invalid IOLTS's (Tretmans: an IOLTS is (Q, L_I, L_U, T, q0)
with Q a countable non-empty set of states, L_I and L_U disjoint label sets,
tau not in L_I or L_U, T a subset of Q x (L_I u L_U u {tau}) x Q and q0 in Q):
  1. Empty state set: Q must be non-empty.
  2. Invalid initial state: q0 is not an element of Q.
  3. Overlapping alphabets: a label is both an input and an output.
  4. Reserved labels in the alphabet: tau (internal action) or delta (quiescence,
     only added by the suspension construction) is declared as input or output.
  5. Dangling transitions: the source or target of a transition is not in Q.
  6. Undeclared labels: a transition uses a label that is not in L_I, L_U or {tau}.

Design choice: the lists in the tuple represent sets, so duplicate entries do not
change the set they denote and are not treated as invalid.
-}

validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initial) =
    not (null states) &&
    initial `elem` states &&
    null (inputs `intersect` outputs) &&
    all (`notElem` [tau, delta]) (inputs ++ outputs) &&
    all validTransition transitions
  where
    validTransition (s, l, s') =
        s `elem` states &&
        s' `elem` states &&
        (l `elem` inputs || l `elem` outputs || l == tau)

-- Helpers to build a fresh state / label that do not occur in a given IOLTS
freshState :: IOLTS -> State
freshState (states, _, _, transitions, initial) =
    1 + maximum (initial : states ++ concatMap (\(s, _, s') -> [s, s']) transitions)

-- Longer than every existing label and contains '#', so it can not be tau or delta
freshLabel :: IOLTS -> Label
freshLabel (_, inputs, outputs, transitions, _) =
    concat (inputs ++ outputs ++ [l | (_, l, _) <- transitions]) ++ "#"

-- Mutations: each one introduces exactly one of the factors listed above
emptyStates, outsideInitial, overlappingAlphabets, tauAsInput, deltaAsOutput,danglingTransition, undeclaredLabel :: IOLTS -> IOLTS
emptyStates (_, i, o, t, q0) = ([], i, o, t, q0)
outsideInitial lts@(q, i, o, t, _) = (q, i, o, t, freshState lts)
overlappingAlphabets lts@(q, i, o, t, q0) = (q, i ++ [l], o ++ [l], t, q0)
    where l = freshLabel lts
tauAsInput (q, i, o, t, q0) = (q, i ++ [tau], o, t, q0)
deltaAsOutput (q, i, o, t, q0) = (q, i, o ++ [delta], t, q0)
danglingTransition lts@(q, i, o, t, q0) = (q, i, o, t ++ [(q0, tau, freshState lts)], q0)
undeclaredLabel lts@(q, i, o, t, q0) = (q, i, o, t ++ [(q0, freshLabel lts, q0)], q0)

invalidatingMutations :: [(String, IOLTS -> IOLTS)]
invalidatingMutations =
    [ ("empty state set", emptyStates)
    , ("initial state outside Q", outsideInitial)
    , ("overlapping alphabets", overlappingAlphabets)
    , ("tau declared as input", tauAsInput)
    , ("delta declared as output", deltaAsOutput)
    , ("dangling transition", danglingTransition)
    , ("undeclared label", undeclaredLabel)
    ]

-- Properties. All of them take a valid IOLTS as input (Exercise 2 provides the generator).

-- Every single invalidating factor on its own makes validateLTS reject the IOLTS
prop_EmptyStatesInvalid, prop_OutsideInitialInvalid, prop_OverlapInvalid, prop_TauLabelInvalid, prop_DeltaLabelInvalid, prop_DanglingInvalid, prop_UndeclaredLabelInvalid :: IOLTS -> Bool
prop_EmptyStatesInvalid = not . validateLTS . emptyStates
prop_OutsideInitialInvalid = not . validateLTS . outsideInitial
prop_OverlapInvalid = not . validateLTS . overlappingAlphabets
prop_TauLabelInvalid = not . validateLTS . tauAsInput
prop_DeltaLabelInvalid = not . validateLTS . deltaAsOutput
prop_DanglingInvalid = not . validateLTS . danglingTransition
prop_UndeclaredLabelInvalid = not . validateLTS . undeclaredLabel

-- Removing any transition from a valid IOLTS keeps it valid
prop_RemoveTransitionValid :: IOLTS -> Bool
prop_RemoveTransitionValid (q, i, o, t, q0) =
    and [validateLTS (q, i, o, delete tr t, q0) | tr <- t]

-- Adding an unreachable fresh state keeps a valid IOLTS valid
prop_AddFreshStateValid :: IOLTS -> Bool
prop_AddFreshStateValid lts@(q, i, o, t, q0) = validateLTS (q ++ [freshState lts], i, o, t, q0)

-- Consistently renaming states with an injective function preserves validity
prop_RenameStatesValid :: IOLTS -> Bool
prop_RenameStatesValid (q, i, o, t, q0) =
    validateLTS (map f q, i, o, [(f s, l, f s') | (s, l, s') <- t], f q0)
  where f s = 2 * s + 7

-- Properties that hold for every valid IOLTS, used by Exercise 2
validProperties :: [(String, IOLTS -> Bool)]
validProperties =
    [ ("prop_EmptyStatesInvalid", prop_EmptyStatesInvalid)
    , ("prop_OutsideInitialInvalid", prop_OutsideInitialInvalid)
    , ("prop_OverlapInvalid", prop_OverlapInvalid)
    , ("prop_TauLabelInvalid", prop_TauLabelInvalid)
    , ("prop_DeltaLabelInvalid", prop_DeltaLabelInvalid)
    , ("prop_DanglingInvalid", prop_DanglingInvalid)
    , ("prop_UndeclaredLabelInvalid", prop_UndeclaredLabelInvalid)
    , ("prop_RemoveTransitionValid", prop_RemoveTransitionValid)
    , ("prop_AddFreshStateValid", prop_AddFreshStateValid)
    , ("prop_RenameStatesValid", prop_RenameStatesValid)
    ]

-- Concrete examples: all IOLTS's provided in LTS.hs are valid
providedModels :: [IOLTS]
providedModels =
    [ counterImpl, counterModel, coffeeImpl1, coffeeModel1, coffeeModel2, coffeeImpl2
    , coffeeImpl3, coffeeModel3, coffeeImpl4, coffeeModel4, coffeeImpl5, coffeeModel5
    , coffeeImpl6, coffeeModel6, tretmanK1, tretmanK2, tretmanK3, tretmanI1, tretmanI2
    , tretmanI3, tretmanI4, tretmanS1, tretmanS2, tretmanS3, tretmanS4, tretmanR1, tretmanR2
    ]

prop_ProvidedModelsValid :: Bool
prop_ProvidedModelsValid = all validateLTS providedModels

-- Runs the properties on the provided models; random testing happens in Exercise 2
main :: IO ()
main = do
    putStrLn "All provided IOLTS's are valid:"
    quickCheck (once prop_ProvidedModelsValid)
    mapM_ (\(name, p) -> putStrLn (name ++ " on provided models:") >> quickCheck (once (all p providedModels)))
          validProperties
