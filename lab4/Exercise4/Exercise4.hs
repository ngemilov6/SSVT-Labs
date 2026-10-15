-- Time Spent: 10 min
module Exercise4 where
import Exercise3
import Data.List
import LTS
import Test.QuickCheck
import Exercise1

after :: IOLTS -> Trace -> [State]
after iolts@(_, _, _, _, initial) trace =
    sort . nub $
        [ s
        | (s, tr) <- nextStep (length trace) iolts [(initial, [])] []
        , tr == trace
        ]

traceTestIOLTS :: IOLTS
traceTestIOLTS =
    ([0, 1, 2], ["coin"], ["tea"], [(0, "coin", 1), (1, "tea", 2)], 0)

traceTestLTS :: LTS
traceTestLTS = (states, inputs ++ outputs, transitions, initialState)
    where
        (states, inputs, outputs, transitions, initialState) = traceTestIOLTS

prop_afterForTraces :: Bool
prop_afterForTraces =
    all (not . null . after traceTestIOLTS) (traces traceTestLTS)

prop_afterForStraces :: Bool
prop_afterForStraces =
    all (not . null . after traceTestIOLTS) (straces traceTestIOLTS)

prop_afterHidesTau :: Bool
prop_afterHidesTau = after tauTestIOLTS ["coin"] == [2]
    where
        tauTestIOLTS =
            ([0, 1, 2], ["coin"], [], [(0, tau, 1), (1, "coin", 2)], 0)