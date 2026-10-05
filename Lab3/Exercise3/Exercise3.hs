module Exercise3 where
import Data.List (subsequences, minimumBy)
import Data.Function (on)

-- Given a list of properties and a list of mutants they kill.
-- Here we represent a mutant's death profile as a list of Bools
minimalSubsets :: Eq mutant => [prop] -> [mutant] -> (prop -> mutant -> Bool) -> [[prop]]
minimalSubsets props mutants killsFunc = 
    let allSubsets = tail (subsequences props)
        
        killedBy subset = filter (\m -> any (`killsFunc` m) subset) mutants
        maxKilled = killedBy props
        
        validSubsets = filter (\s -> killedBy s == maxKilled) allSubsets
        minLen = minimum (map length validSubsets)
        
    in filter (\s -> length s == minLen) validSubsets

