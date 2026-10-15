-- Additional dependencies: none
-- Time Spent: 90 min
module Exercise3 where
import Data.List
import LTS
import Test.QuickCheck
import Exercise1
import Exercise2

straces :: IOLTS -> [Trace]
straces = stracesUpTo 10

stracesUpTo :: Int -> IOLTS -> [Trace]
stracesUpTo maxLen iolts@(states, inputs, outputs, transitions, initial) =
    nub $ map snd (nextStep maxLen iolts [(initial, [])] [])

moves :: IOLTS -> (State, Trace) -> [(State, Trace)]
moves iolts@(states, inputs, outputs, transitions, initial) (q, trace) =
    normal ++ tauMove ++ deltaMove
    where
        normal = [(s', trace ++ [l]) | (s, l, s') <- transitions, s == q, l /= tau]
        tauMove = [(s', trace) | (s, l, s') <- transitions, s == q, l == tau]
        deltaMove = if isQuiscent iolts q && (null trace || last trace /= delta) then [(q, trace ++ [delta])] else []


isQuiscent :: IOLTS -> State -> Bool
isQuiscent (states, inputs, outputs, transitions, initial) state = 
    null [t | t@(s,o,_) <- transitions, s== state, (o == tau || o `elem` outputs)]


-- Swtih to lazyness use main to take 10-20
nextStep :: Int -> IOLTS -> [(State, Trace)] -> [(State, Trace)] -> [(State, Trace)]
nextStep maxLen iolts current seen
    | null current = seen
    | otherwise = nextStep maxLen iolts next (seen ++ current)
    where
        next = [t | t@(s, tr) <- concatMap (moves iolts) current, length tr <= maxLen, t `notElem` seen]


randomTraceGen :: Gen (IOLTS, Trace)
randomTraceGen = do
    iolts <- Exercise2.ltsGen
    let tr = stracesUpTo 4 iolts
    trace <- elements tr
    return (iolts, trace)