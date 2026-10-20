-- Additional dependencies: none
-- Time Spent: 80 min
module Exercise5 where
import Data.List
import LTS
import Test.QuickCheck
import Control.Exception
import System.IO.Unsafe



doorModel :: LTS
doorModel = createLTS
    [ (0, "?close", 3), (3, "!closed", 1)
    , (1, "?open", 4),  (4, "!opened", 0)
    , (1, "?lock", 5),  (5, "!locked", 2)
    , (2, "?unlock", 6), (6, "!unlocked", 1)
    ]


testLTSAgainstSUT :: LTS -> (State -> Label -> (State, Label)) -> Bool
testLTSAgainstSUT lts sut = all (testTrace sut) (getTraces lts)

getTraces :: LTS -> [Trace]
getTraces lts = takeWhile (\t -> length t <= 16) (traces lts)

testTrace :: (State -> Label -> (State, Label)) -> Trace -> Bool
testTrace sut trace = case catchError (testFromTrace sut trace 0) of
    Left msg -> error (msg)
    Right ok -> ok

catchError :: Bool -> Either String Bool
catchError x = unsafePerformIO $ do
    result <- try (evaluate x) :: IO (Either ErrorCall Bool)
    return (either (Left . takeWhile (/= '\n') . displayException) Right result)
{-# NOINLINE catchError #-}

testFromTrace :: (State -> Label -> (State, Label)) -> Trace -> State -> Bool
testFromTrace sut [] state = True
testFromTrace sut [input] state = True
testFromTrace sut (input : output : rest) state =
    case catchError ("!" ++ newOutput == output) of
        Left msg -> error ("after input " ++ input ++ " expected " ++ output ++ " but the SUT threw an error in state " ++ show state ++ ": " ++ msg)
        Right True  -> testFromTrace sut rest newState
        Right False -> error ("after input " ++ input ++ " expected " ++ output ++ " but got !" ++ newOutput)
    where (newState, newOutput) = sut state (drop 1 input)
