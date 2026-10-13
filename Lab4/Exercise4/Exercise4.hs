module Exercise4 where

import Data.List (foldl', nub)
import LTS
import Test.QuickCheck
import qualified Exercise3

-- run with: runghc -i".." -i"..\Exercise1" -i"..\Exercise2" -i"..\Exercise3" Exercise4.hs

-- Return the states reachable after an observable trace, hiding internal tau steps
after :: IOLTS -> Trace -> [State]
after (_, _, outputs, transitions, initialState) trace =
	foldl' advance (tauClosure [initialState]) trace
	where
		advance currentStates label
			| label == delta = filter isQuiescent currentStates
			| label == tau = []
			| otherwise = tauClosure
				[target | (source, action, target) <- transitions
				, source `elem` currentStates
				, action == label]
		isQuiescent state =
			not (any (hasOutputOrTauFrom state) transitions)
		hasOutputOrTauFrom state (source, label, _) =
			source == state && (label == tau || label `elem` outputs)
		tauClosure seeds = close (nub seeds)
			where
				close reached =
					let expanded = nub (reached ++
						[target | (source, label, target) <- transitions
						, source `elem` reached
						, label == tau])
					in if length expanded == length reached then reached else close expanded

-- A small acyclic system for checking the traces helper
traceTestIOLTS :: IOLTS
traceTestIOLTS = ([0, 1, 2], ["coin"], ["tea"], [(0, "coin", 1), (1, "tea", 2)], 0)

traceTestLTS :: LTS
traceTestLTS = (states, inputs ++ outputs, transitions, initialState)
	where
		(states, inputs, outputs, transitions, initialState) = traceTestIOLTS

-- Check that all ordinary traces lead to at least one state
prop_afterForTraces :: Bool
prop_afterForTraces = all (not . null . after traceTestIOLTS) (traces traceTestLTS)

-- Check that bounded suspension traces lead to at least one state
prop_afterForStraces :: Bool
prop_afterForStraces = all (not . null . after traceTestIOLTS) (take 30 (Exercise3.straces traceTestIOLTS))

-- Check that tau steps are hidden when finding states after a trace
prop_afterHidesTau :: Bool
prop_afterHidesTau = after tauTestIOLTS ["coin"] == [2]
	where
		tauTestIOLTS = ([0, 1, 2], ["coin"], [], [(0, tau, 1), (1, "coin", 2)], 0)

-- Check that delta leads only to quiescent states
prop_afterHandlesDelta :: Bool
prop_afterHandlesDelta = after traceTestIOLTS ["coin", "tea", delta] == [2]

-- Check that generated suspension traces lead to at least one state
prop_afterForGeneratedStraces :: Property
prop_afterForGeneratedStraces = forAll Exercise3.randomTraceGen $ \(iolts, trace) ->
	not (null (after iolts trace))

main :: IO ()
main = do
	quickCheck prop_afterForTraces
	quickCheck prop_afterForStraces
	quickCheck prop_afterHidesTau
	quickCheck prop_afterHandlesDelta
	quickCheck prop_afterForGeneratedStraces
