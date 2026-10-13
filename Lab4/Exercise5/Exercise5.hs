module Exercise5 where

import Control.Exception (ErrorCall, SomeException, displayException, evaluate, try)
import Data.List (intercalate)
import LTS
import System.IO.Unsafe (unsafePerformIO)

-- Model door inputs and their expected outputs as alternating LTS transitions
doorModel :: LTS
doorModel =
	([0, 1, 2, 3, 4, 5, 6], labels, transitions, 0)
	where
		labels = ["?close", "!closed", "?open", "!opened", "?lock", "!locked", "?unlock", "!unlocked"]
		transitions =
			[(0, "?close", 3), (3, "!closed", 1)
			,(1, "?open", 4), (4, "!opened", 0)
			,(1, "?lock", 5), (5, "!locked", 2)
			,(2, "?unlock", 6), (6, "!unlocked", 1)]

-- Check all specified commands without comparing model states to SUT states
testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT (_, _, transitions, initialModelState) sut =
	explore [(0, initialModelState, [])] []
	where
		-- Each queued case tracks SUT state, model state, and commands used to reach them
		explore [] _ = True
		explore ((sutState, modelState, history):pending) visited
			-- Repeated state pairs need no retesting, even when model and SUT state numbers differ
			| (sutState, modelState) `elem` visited = explore pending visited
			| otherwise = checkCommands commands []
				where
					visited' = (sutState, modelState) : visited
					-- Input edges identify commands, with the following state holding their expected output
					commands =
						[(command, responseState)
						| (source, '?':command, responseState) <- transitions
						, source == modelState]
					checkCommands [] discovered = explore (pending ++ reverse discovered) visited'
					checkCommands ((command, responseState):remaining) discovered =
						-- Match the SUT response against the model before exploring the next state
						case [(output, target)
							| (source, '!':output, target) <- transitions
							, source == responseState] of
							[] -> error ("Invalid door model: command " ++ show command ++ " has no expected output")
							expectedOutputs ->
								case invokeSUT sut sutState command of
									Left failure -> error (formatSUTFailure history command failure)
									Right (nextSutState, actualOutput) ->
										case [target | (output, target) <- expectedOutputs, output == actualOutput] of
											[] -> error (formatMismatch history command expectedOutputs actualOutput)
											matchingTargets -> checkCommands remaining
												([(nextSutState, target, history ++ [command]) | target <- matchingTargets] ++ discovered)
		formatSUTFailure history command failure =
			"SUT raised an error after commands " ++ show (history ++ [command])
				++ ": " ++ takeWhile (/= '\n') failure
		formatMismatch history command expectedOutputs actualOutput =
			"SUT mismatch after commands " ++ show (history ++ [command])
				++ ": expected output " ++ intercalate " or " (map (show . fst) expectedOutputs)
				++ " but received " ++ show actualOutput

-- Capture errors from the pure SUT so they can include the tested command trace
invokeSUT :: (State -> Label -> (State, Label)) -> State -> Label -> Either String (State, Label)
invokeSUT sut state command = unsafePerformIO $ do
	-- Force the returned state and output while the exception handler is active
	result <- try (evaluate (forceResult (sut state command))) :: IO (Either ErrorCall (State, Label))
	pure (either (Left . displayException) Right result)
	where
		forceResult (nextState, output) = length output `seq` nextState `seq` (nextState, output)
{-# NOINLINE invokeSUT #-}

-- Report whether one door implementation satisfies the model
reportDoor :: String -> (State -> Label -> (State, Label)) -> IO ()
reportDoor name sut = do
	result <- try (evaluate (testLTSAgainstSUT doorModel sut)) :: IO (Either SomeException Bool)
	case result of
		Right True -> putStrLn (name ++ ": PASS")
		Right False -> putStrLn (name ++ ": FAIL")
		-- Keep the concise failure reason and omit the compiler call stack
		Left failure -> putStrLn (name ++ ": " ++ takeWhile (/= '\n') (displayException failure))

main :: IO ()
main = do
	reportDoor "doorImpl1" doorImpl1
	reportDoor "doorImpl2" doorImpl2
	reportDoor "doorImpl3" doorImpl3
	reportDoor "doorImpl4" doorImpl4
	reportDoor "doorImpl5" doorImpl5
	reportDoor "doorImpl6" doorImpl6
	reportDoor "doorImpl7" doorImpl7
	reportDoor "doorImpl8" doorImpl8
