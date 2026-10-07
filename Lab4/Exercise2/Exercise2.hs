module Exercise2 where

import Data.List
import LTS
import Test.QuickCheck
import qualified Exercise1

-- run with: runghc --ghc-arg=-i../:../Exercise1 Exercise2.hs +RTS -M512M -RTS

-- Generate a valid IOLTS with random states, alphabets, and transitions
ltsGen :: Gen IOLTS
ltsGen = do
	stateCount <- choose (1, 5) :: Gen Int
	inputCount <- choose (0, 3) :: Gen Int
	outputCount <- choose (0, 3) :: Gen Int
	transitionCount <- choose (0, 10) :: Gen Int
	let
		states = map fromIntegral [0 .. stateCount - 1]
		inputs = ["input" ++ show index | index <- [1 .. inputCount]]
		outputs = ["output" ++ show index | index <- [1 .. outputCount]]
		actionLabels = inputs ++ outputs ++ [tau]
	transitions <- vectorOf transitionCount $
		(,,) <$> elements states <*> elements actionLabels <*> elements states
	pure (states, inputs, outputs, transitions, 0)

-- Property to test that generated IOLTS values satisfy the validator
prop_ltsGenProducesValidIOLTS :: Property
prop_ltsGenProducesValidIOLTS = forAll ltsGen Exercise1.validateLTS

main :: IO ()
main = do
	quickCheck prop_ltsGenProducesValidIOLTS
	quickCheck Exercise1.prop_validateLTS_acceptsValid
	quickCheck Exercise1.prop_validateLTS_rejectsEmptyStates
	quickCheck Exercise1.prop_validateLTS_rejectsMissingInitialState
	quickCheck Exercise1.prop_validateLTS_rejectsUndeclaredTransitionState
	quickCheck Exercise1.prop_validateLTS_rejectsUndeclaredTransitionLabel
	quickCheck Exercise1.prop_validateLTS_rejectsOverlappingAlphabets
	quickCheck Exercise1.prop_validateLTS_rejectsTauInInputs
	quickCheck Exercise1.prop_validateLTS_rejectsTauInOutputs


