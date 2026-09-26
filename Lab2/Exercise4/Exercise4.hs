module Exercise4 where

import Data.List
import Test.QuickCheck

type Rel a = [(a,a)]

hasRelation :: Eq a => [a] -> Rel a -> a -> Bool
hasRelation domain rel n = any (\(x,y) -> x == n && elem y domain) rel

isSerial :: Eq a => [a] -> Rel a -> Bool
isSerial domain rel = all (\n -> hasRelation domain rel n) domain

--- Mention in documentation base cases where domain is empty and/or relation is empty

--- Helper to check whether all elements that are part of a relation are part of the domain
isInDomain :: Eq a => [a] -> Rel a -> Bool
isInDomain domain rel = all (\(a,b) -> elem a domain && elem b domain) rel 

--- Generate random relation from a domain using QuickCheck
genRelFromDomain :: [Integer] -> Gen (Rel Integer)
genRelFromDomain domain
  | null domain = pure []
  | otherwise =
      listOf ((,) <$> elements domain <*> elements domain)

--- Generate an arbitrary Domain and Relation using domain
genDomainRel :: Gen ([Integer], Rel Integer)
genDomainRel = do
    domain <- arbitrary
    rel <- genRelFromDomain domain
    pure (domain, rel)

--- Property that tests whether all elements are in the domain
prop_domain :: Property
prop_domain =
    forAllShrink genDomainRel (const []) $ \(domain, rel) ->
        isInDomain domain rel ==> isSerial domain rel

--- Property that tests  whether isInDomain is correct for 
--- identifying an additional element that is not part of the domain
prop_notInDomain :: Property
prop_notInDomain =
    forAll (arbitrary :: Gen [Integer]) $ \domain ->
      forAll (arbitrary :: Gen Integer) $ \x ->
        x `notElem` domain ==>
          not (isInDomain domain [(x, x)])

--- Property that tests that a relation is serial if all reflexive relations are added
prop_identitiesSerial :: [Integer] -> Rel Integer -> Bool
prop_identitiesSerial domain rel =
    isSerial domain (rel ++ [(x, x) | x <- domain])

--- Property that tests that a relation where one isolated element exists is not serial
prop_isolatedNotSerial :: Property
prop_isolatedNotSerial =
    forAll (listOf1 (arbitrary :: Gen Integer)) $ \xs ->
        let domain = nub xs
            missing = head domain
            rel = [(x, x) | x <- tail domain]
        in
            not (isSerial domain rel)

main :: IO()
main = do
    print (isSerial [1,2,3] [(1,4), (2,3)])
    print (isSerial [1,2,3] [(1,4), (2,3), (3,1)])
    quickCheck prop_domain
    quickCheck prop_notInDomain
    quickCheck prop_identitiesSerial
    quickCheck prop_isolatedNotSerial