-- Additional dependencies: none
-- Time Spent: X min
module Exercise1 where
import Data.List
import LTS
import Test.QuickCheck


badTransitionTarget :: IOLTS
badTransitionTarget = ([1,2,3], ["coin"], ["coffee"], [(1,"coin",7)], 1)

badTransitionLabel :: IOLTS
badTransitionLabel = ([1,2,3], ["coin"], ["coffee"], [(1,"tea",2)], 1)


validateLTS :: IOLTS -> Bool
validateLTS (states, inputs, outputs, transitions, initial) =
    not (null states) 
    && initial `elem` states 
    && length (intersect inputs outputs) == 0
    && all (\(a,_,b) -> a `elem` states && b `elem` states) transitions
    && all (\(_,l,_) -> l `elem` inputs || l `elem` outputs || l == tau) transitions

