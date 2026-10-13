module Exercise3 where

import Data.List (nub)
import LTS
import Test.QuickCheck
import qualified Exercise2

-- Find suspension traces with BFS, adding delta steps for quiescence
straces :: IOLTS -> [Trace]
straces (states, inputs, outputs, transitions, initialState) =
	-- Begin with every state reachable from the initial state through tau transitions
	enumerate [(tauClosure [initialState], [])]
	where
		observableLabels = nub (inputs ++ outputs)
		enumerate [] = []
		-- Expand the current trace one observable action at a time
		enumerate frontier = map snd frontier ++ enumerate nextFrontier
			where
				nextFrontier =
					[(nextStates, trace ++ [label])
					| (currentStates, trace) <- frontier
					, (label, nextStates) <- successors currentStates]
		successors currentStates = visibleSuccessors ++ deltaSuccessor
			where
				-- A visible action may follow any tau path and may itself be followed by tau paths
				visibleSuccessors =
					[(label, destinations)
					| label <- observableLabels
					, let destinations = tauClosure
							[target | (source, action, target) <- transitions
							, source `elem` currentStates
							, action == label]
					, not (null destinations)]
				-- Delta is possible when a reachable state is stable and has no output enabled
				quiescentStates = filter isQuiescent currentStates
				deltaSuccessor
					| null quiescentStates = []
					| otherwise = [(delta, quiescentStates)]
		isQuiescent state =
			not (any (hasOutputOrTauFrom state) transitions)
		hasOutputOrTauFrom state (source, label, _) =
			source == state && (label == tau || label `elem` outputs)
		-- Follow internal transitions until the reachable state set stops growing
		tauClosure seeds = close (nub seeds)
			where
				close reached =
					let expanded = nub (reached ++
						[target | (source, label, target) <- transitions
						, source `elem` reached
						, label == tau])
					in if length expanded == length reached then reached else close expanded

-- Generate an IOLTS and one of its first thirty suspension traces
randomTraceGen :: Gen (IOLTS, Trace)
randomTraceGen = do
	iolts <- Exercise2.ltsGen
	let candidates = take 30 (straces iolts)
	trace <- elements candidates
	pure (iolts, trace)

-- Property to test that every IOLTS has the empty suspension trace
prop_stracesContainEmpty :: Property
prop_stracesContainEmpty = forAll Exercise2.ltsGen $ \iolts ->
	[] `elem` take 30 (straces iolts)

-- Property to test that internal tau labels are hidden from suspension traces
prop_stracesHideTau :: Property
prop_stracesHideTau = forAll Exercise2.ltsGen $ \iolts ->
	all (notElem tau) (take 30 (straces iolts))

-- Property to test that traces use only visible actions or delta
prop_stracesUseSuspensionAlphabet :: Property
prop_stracesUseSuspensionAlphabet = forAll Exercise2.ltsGen $ \iolts@(_, inputs, outputs, _, _) ->
	all (`elem` (inputs ++ outputs ++ [delta])) (concat (take 30 (straces iolts)))

-- Property to test that the random trace generator returns a suspension trace
prop_randomTraceComesFromStraces :: Property
prop_randomTraceComesFromStraces = forAll randomTraceGen $ \(iolts, trace) ->
	trace `elem` take 30 (straces iolts)

main :: IO ()
main = do
	quickCheck prop_stracesContainEmpty
	quickCheck prop_stracesHideTau
	quickCheck prop_stracesUseSuspensionAlphabet
	quickCheck prop_randomTraceComesFromStraces
