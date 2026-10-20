module Exercise5 where
import Data.List
import LTS
import Exercise1 (validateLTS)
import Exercise4 (after)
import Control.Exception (SomeException, try, evaluate, displayException)
import Data.Functor.Identity (runIdentity)

{-
The specification is an LTS whose labels keep their '?' (input) / '!' (output)
prefix, as createLTS does. One call of the SUT (State -> Label -> (State, Label))
is an input followed by the output it returns, so the model alternates: from a
"stable" state an input leads to an intermediate state that allows the expected
output(s).

The LTS is converted to an IOLTS (ltsToIOLTS), so we can reuse the earlier exercises:
validateLTS (Exercise 1) rejects an invalid model, and `after` (Exercise 4, built on
the tau-closure and quiescence of Exercise 3) gives the model states after a trace.
The check is then literally the ioco definition: the SUT output after trace sigma
must be in out(model `after` sigma), where out includes delta if a quiescent state
is reached.

testLTSAgainstSUT explores ALL input sequences that the model allows, up to
maxDepth inputs. It uses iterative deepening (depth 1, 2, ... maxDepth), so the
first failure that is reported has the shortest possible trace, which makes the
error message easy to read. It keeps the SUT state and the trace so far. For every input
in the model `after` the trace it calls the SUT and checks that the returned output
is in out(model `after` (trace ++ [input])). The SUT is only given inputs the model specifies.

The exploration is written for any monad so that two runners share it:
 - testLTSAgainstSUT (pure, the required signature): returns True or throws an
   error describing the trace, the expected outputs and the actual output.
   A crash inside the SUT itself can not be caught in pure code and is propagated.
 - reportLTSAgainstSUT (IO): additionally catches crashes of the SUT and reports
   on which trace and input it crashed.

Bugs found (verified with main):
 * doorImpl2: the outputs of close and open are swapped: close answers "opened",
   open answers "closed".
 * doorImpl3: unlock answers "unlocked" but stays in internal state 2 (still
   locked), so the next open or lock crashes.
 * doorImpl4: the lock and unlock commands are swapped: lock on a closed door
   crashes, while unlock is accepted on a closed door and answers "locked".
 * doorImpl5: open answers "open" instead of "opened".
 * doorImpl6: the door gets stuck: after close, open, close, open, close the next
   open throws "Door is stuck!" (locking and unlocking resets the counter).
 * doorImpl7: after unlocking once, then opening, closing and locking again, the
   next unlock throws "Incorrect keycode!".
 * doorImpl8: after unlocking and two open/close cycles the door is in internal
   state 7, where open is not implemented (crash). It also accepts close on a
   closed door, which ioco can not observe because that input is not specified.
-}

type SUT = State -> Label -> (State, Label)

-- Correct door behaviour (doorImpl1). States 0 (open), 2 (closed), 5 (locked) are
-- stable; 1, 3, 4, 6 are the intermediate states that wait for the output.
doorModel :: LTS
doorModel = createLTS
    [ (0, "?close", 1), (1, "!closed", 2)
    , (2, "?open", 3), (3, "!opened", 0)
    , (2, "?lock", 4), (4, "!locked", 5)
    , (5, "?unlock", 6), (6, "!unlocked", 2)
    ]

maxDepth :: Int
maxDepth = 10

-- An LTS with '?'/'!' labels as an IOLTS (like createIOLTS, but keeping the states
-- and initial state of the given LTS), so the functions of Exercises 1, 3 and 4 apply
ltsToIOLTS :: LTS -> IOLTS
ltsToIOLTS (states, labels, transitions, initial) =
    (states, withPrefix '?', withPrefix '!', [(s, makeLabel l, s') | (s, l, s') <- transitions], initial)
  where withPrefix c = [drop 1 l | l <- labels, take 1 l == [c]]

-- Inputs the model allows after a trace
inputsAfter :: IOLTS -> Trace -> [Label]
inputsAfter model@(_, inputs, _, _, _) trace =
    [i | i <- inputs, not (null (model `after` (trace ++ [i])))]

-- out(model after trace) from ioco: the allowed outputs, plus delta if quiescence is allowed
outAfter :: IOLTS -> Trace -> [Label]
outAfter model@(_, _, outputs, _, _) trace =
    [o | o <- outputs ++ [delta], not (null (model `after` (trace ++ [o])))]

-- Shows labels with their '?' / '!' prefix again
showTrace :: IOLTS -> Trace -> String
showTrace _ [] = "(empty trace)"
showTrace model trace = unwords (map (showLabel model) trace)

showLabel :: IOLTS -> Label -> String
showLabel (_, inputs, outputs, _, _) l
    | l `elem` inputs = '?' : l
    | l `elem` outputs = '!' : l
    | otherwise = l

-- Shared exploration. callSut returns Left with a message if the SUT crashed.
checkLTSAgainstSUT :: Monad m => (State -> Label -> m (Either String (State, Label))) -> LTS -> m (Either String ())
checkLTSAgainstSUT callSut lts
    | not (validateLTS model) = return (Left "The model is not a valid IOLTS (see Exercise 1)")
    | otherwise = firstFailure [explore 0 [] depth | depth <- [1 .. maxDepth]]
  where
    model = ltsToIOLTS lts
    showOuts = show . map (showLabel model)

    explore sutState trace depth
        | depth == 0 = return (Right ())
        | otherwise = firstFailure [tryInput sutState trace depth input | input <- inputsAfter model trace]

    tryInput sutState trace depth input = do
        let trace' = trace ++ [input]
            expected = outAfter model trace'
        result <- callSut sutState input
        case result of
            Left crash -> return $ Left $
                "SUT crashed after trace " ++ showTrace model trace ++ " on input ?" ++ input ++ "\n" ++
                "  expected one of the outputs " ++ showOuts expected ++ "\n" ++
                "  SUT error: " ++ crash
            Right (sutState', output)
                | output `elem` expected -> explore sutState' (trace' ++ [output]) (depth - 1)
                | otherwise -> return $ Left $
                    "SUT does not conform (ioco) after trace " ++ showTrace model trace' ++ "\n" ++
                    "  expected one of the outputs " ++ showOuts expected ++ "\n" ++
                    "  but the SUT produced !" ++ output

    firstFailure [] = return (Right ())
    firstFailure (test : tests) = test >>= either (return . Left) (const (firstFailure tests))

-- Required function: True if the SUT conforms, otherwise a descriptive error
testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT model sut =
    either error (const True) $ runIdentity $
        checkLTSAgainstSUT (\s l -> return (Right (sut s l))) model

-- IO variant that also turns crashes of the SUT into a descriptive report
reportLTSAgainstSUT :: LTS -> SUT -> IO (Either String ())
reportLTSAgainstSUT model sut = checkLTSAgainstSUT safeCall model
  where
    safeCall s l = do
        result <- try (evaluate (forceResult (sut s l)))
        return $ case result of
            Left e -> Left (takeWhile (/= '\n') (displayException (e :: SomeException)))
            Right r -> Right r
    forceResult r@(s', o) = s' `seq` length o `seq` r

doors :: [(String, SUT)]
doors = [ ("doorImpl1", doorImpl1), ("doorImpl2", doorImpl2), ("doorImpl3", doorImpl3)
        , ("doorImpl4", doorImpl4), ("doorImpl5", doorImpl5), ("doorImpl6", doorImpl6)
        , ("doorImpl7", doorImpl7), ("doorImpl8", doorImpl8) ]

main :: IO ()
main = do
    putStrLn "testLTSAgainstSUT doorModel doorImpl1:"
    print (testLTSAgainstSUT doorModel doorImpl1)
    putStrLn ""
    mapM_ (\(name, sut) -> do
              result <- reportLTSAgainstSUT doorModel sut
              putStrLn $ name ++ ": " ++ either ("FAIL\n  " ++) (const "PASS") result)
          doors
