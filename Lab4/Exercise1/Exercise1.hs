module Exercise1 where

import Data.List
import LTS
import Test.QuickCheck


-- An IOLTS is invalid if:

-- The state set is empty, or the initial state is not in it
-- A transition starts or ends at a state not in the state set
-- A transition label is neither an input label, an output label, nor the internal label
-- An action label appears in both the input and output sets; those sets must be disjoint
-- The internal label tau must not appear in the input or output sets

validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initialState) =
	-- The state set must be nonempty and contain the initial state
	not (null states)
		&& initialState `elem` states
		-- Input and output labels are disjoint; tau is reserved for internal transitions
		&& null (inputs `intersect` outputs)
		&& tau `notElem` inputs
		&& tau `notElem` outputs
		-- Every transition must use declared states and an allowed label
		&& all validTransition transitions
  where
	validTransition (source, label, target) =
		source `elem` states
			&& target `elem` states
			&& (label == tau || label `elem` inputs || label `elem` outputs)

-- A valid example of an IOLTS to use in testing
validIOLTS :: IOLTS
validIOLTS = ([0, 1, 2], ["request"], ["response"], [(0, "request", 1), (1, "response", 2), (2, tau, 0)], 0)

-- Property to test that a valid IOLTS is accepted
prop_validateLTS_acceptsValid :: Bool
prop_validateLTS_acceptsValid = validateLTS validIOLTS

-- Property to test that an IOLTS with no states is rejected
prop_validateLTS_rejectsEmptyStates :: Bool
prop_validateLTS_rejectsEmptyStates = not (validateLTS ([], [], [], [], 0))

-- Property to test that an initial state outside the state set is rejected
prop_validateLTS_rejectsMissingInitialState :: Bool
prop_validateLTS_rejectsMissingInitialState = not (validateLTS ([0], [], [], [], 1))

-- Property to test that a transition to an undeclared state is rejected
prop_validateLTS_rejectsUndeclaredTransitionState :: Bool
prop_validateLTS_rejectsUndeclaredTransitionState = not (validateLTS ([0], ["request"], [], [(0, "request", 1)], 0))

-- Property to test that a transition with an undeclared label is rejected
prop_validateLTS_rejectsUndeclaredTransitionLabel :: Bool
prop_validateLTS_rejectsUndeclaredTransitionLabel = not (validateLTS ([0, 1], [], [], [(0, "unknown", 1)], 0))

-- Property to test that input and output alphabets with a shared label are rejected
prop_validateLTS_rejectsOverlappingAlphabets :: Bool
prop_validateLTS_rejectsOverlappingAlphabets = not (validateLTS ([0], ["action"], ["action"], [], 0))

-- Property to test that tau in the input alphabet is rejected
prop_validateLTS_rejectsTauInInputs :: Bool
prop_validateLTS_rejectsTauInInputs = not (validateLTS ([0], [tau], [], [], 0))

-- Property to test that tau in the output alphabet is rejected
prop_validateLTS_rejectsTauInOutputs :: Bool
prop_validateLTS_rejectsTauInOutputs = not (validateLTS ([0], [], [tau], [], 0))


