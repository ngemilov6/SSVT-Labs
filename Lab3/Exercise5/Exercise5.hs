module Exercise5 where

import Data.List (nub)

type PropId = Int
type KilledMutants = [Int]

equivalentProperties :: [(PropId, KilledMutants)] -> [[PropId]]
equivalentProperties results = 
    let groups = [ map fst (filter (\(_, m2) -> m1 == m2) results) | (_, m1) <- results ]
    in nub $ filter (\g -> length g > 1) groups

subsumedProperties :: [(PropId, KilledMutants)] -> [(PropId, PropId)]
subsumedProperties results = 
    [ (pStrong, pWeak) 
    | (pStrong, mStrong) <- results, (pWeak, mWeak) <- results
    , pStrong /= pWeak
    , all (`elem` mStrong) mWeak
    , length mStrong > length mWeak 
    ]