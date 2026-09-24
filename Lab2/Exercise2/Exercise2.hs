module Exercise2 where

import Test.QuickCheck
import SetOrd
import Exercise1 (genSet, genSetQuickCheck)
import Control.Monad (replicateM)

-- run with:
-- runghc --ghc-arg=-i..;../Exercise1 Exercise2.hs +RTS -M512M -RTS

-- Operator for union of two sets
setUnion :: (Ord a) => Set a -> Set a -> Set a
setUnion (Set xs) (Set ys) = list2set (xs ++ ys)

-- Operator for intersection of two sets
setIntersection :: (Ord a) => Set a -> Set a -> Set a
setIntersection (Set xs) (Set ys) = list2set [x | x <- xs, x `elem` ys]

-- Operator for difference of two sets
setDifference :: (Ord a) => Set a -> Set a -> Set a
setDifference (Set xs) (Set ys) = list2set [x | x <- xs, x `notElem` ys]

-- Property to test that the union of two sets is commutative
propUnionCummutative :: Set Int -> Set Int -> Bool
propUnionCummutative set1 set2 = setUnion set1 set2 == setUnion set2 set1

-- Property to test that the intersection of two sets is commutative
propIntersectionCummutative :: Set Int -> Set Int -> Bool
propIntersectionCummutative set1 set2 = setIntersection set1 set2 == setIntersection set2 set1

-- Property to test that the difference of two sets is not commutative
propDifferenceNonCummutative :: Set Int -> Set Int -> Property
propDifferenceNonCummutative set1 set2 =
    condition ==> differenceIsDifferent
  where
    -- Condition to ensure that the sets are not equal and not empty
    condition =
        not (isEmpty set1)
        && not (isEmpty set2)
        && set1 /= set2
        && not (subSet set1 set2)
        && not (subSet set2 set1)

    differenceIsDifferent =
        setDifference set1 set2 /= setDifference set2 set1

-- Property to test that the union of a set with itself is idempotent
propIdempotentUnion :: Set Int -> Bool
propIdempotentUnion set = setUnion set set == set

-- Property to test that the intersection of a set with itself is idempotent
propIdempotentIntersection :: Set Int -> Bool
propIdempotentIntersection set = setIntersection set set == set

-- Property to test that the difference of a set with itself is empty
propDifferenceSelf :: Set Int -> Bool
propDifferenceSelf set =
    setDifference set set == emptySet

main ::  IO ()
main = do
    -- TODO: Tests on two different domains?

    -- 100 random tests using the same properties as the QuickCheck checks
    results <- replicateM 100 $ do
        set1 <- genSet 10
        set2 <- genSet 10
        let unionProp = propUnionCummutative set1 set2
            intersectionProp = propIntersectionCummutative set1 set2
            differenceProp = setDifference set1 set2 /= setDifference set2 set1
            idempotentUnionProp = propIdempotentUnion set1
            idempotentIntersectionProp = propIdempotentIntersection set1
            differenceSelfProp = propDifferenceSelf set1
        return $ unionProp
            && intersectionProp
            && differenceProp
            && idempotentUnionProp
            && idempotentIntersectionProp
            && differenceSelfProp
    print ("Tests passed:" ++ show (and results))

    -- QuickCheck tests
    quickCheck propUnionCummutative
    quickCheck propIntersectionCummutative
    quickCheck propDifferenceNonCummutative
    quickCheck propIdempotentUnion
    quickCheck propIdempotentIntersection
    quickCheck propDifferenceSelf