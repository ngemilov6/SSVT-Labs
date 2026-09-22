module Exercise7 where

import Lecture3
import Prelude 
import Test.QuickCheck

-- 1. Eliminate Impl / Equiv (arrowfree from Lecture3)
-- 2. Push negations inward (nnf from Lecture3)
-- 3. Distribute disjunction over conjunction (toCNF)
cnf :: Form -> Form
cnf = toCNF . nnf . arrowfree 

-- recursively structures the formula into Conjunctive Normal Form
toCNF :: Form -> Form
toCNF (Cnj fs) = Cnj (mergeCnj (map toCNF fs))
toCNF (Dsj []) = Dsj []
toCNF (Dsj (f:fs)) = foldl dist (toCNF f) (map toCNF fs)
toCNF f = f

-- merges nested conjunctions on the same structural level
mergeCnj :: [Form] -> [Form]
mergeCnj [] = []
mergeCnj (x:xs) = case x of
  Cnj forms -> forms ++ mergeCnj xs
  form -> [form] ++ mergeCnj xs

-- distributes disjunctions over conjunctions
dist :: Form -> Form -> Form
dist (Cnj fs1) (Cnj fs2) = Cnj [dist f1 f2 | f1<- fs1, f2<-fs2]
dist (Cnj fs1) f2 = Cnj [dist f1 f2 | f1 <- fs1]
dist f1 (Cnj fs2) = Cnj [dist f1 f2 | f2 <- fs2]
dist (Dsj fs1) (Dsj fs2) = Dsj (fs1 ++ fs2)
dist (Dsj fs1) f2 = Dsj (fs1 ++ [f2])
dist f1 (Dsj fs2) = Dsj (f1 : fs2)
dist f1 f2 = Dsj [f1, f2]

-- generator to build random formulas for testing
instance Arbitrary Form where
  arbitrary = sized (\n -> genForm (min n 6)) -- Cap the max depth to prevent stack overflow
    where
      genForm 0 = Prop <$> elements [1..3]
      genForm n = oneof
        [ Prop <$> elements [1..3],
          Neg <$> genForm (n `div` 2),
          Cnj <$> resize 2 (listOf1 (genForm (n `div` 2))), 
          Dsj <$> resize 2 (listOf1 (genForm (n `div` 2))),
          Impl <$> genForm (n `div` 2) <*> genForm (n `div` 2),
          Equiv <$> genForm (n `div` 2) <*> genForm (n `div` 2)
        ]

-- property that verifies a formula evaluates exactly identically to its CNF counterpart across all possible valuations
prop_cnf_equiv :: Form -> Bool
prop_cnf_equiv f = all (\v -> evl v f == evl v f') (allVals f)
  where f' = cnf f

-- manual examples for targeted testing
manualExamples :: [Form]
manualExamples = [
    Equiv (Impl (Prop 1) (Prop 2)) (Dsj [Neg (Prop 3), Prop 4]),
    Dsj [Cnj [Prop 1, Prop 2], Cnj [Neg (Prop 3), Prop 4]],
    Dsj [Prop 1, Cnj [Prop 2, Dsj [Neg (Prop 3), Cnj [Prop 4, Prop 5]]]],
    Dsj [Dsj [Prop 1, Cnj [Prop 2, Neg (Prop 3)]], Cnj [Prop 4, Prop 5]],
    Dsj [Cnj [Prop 1, Neg (Prop 2)], Cnj [Neg (Prop 3), Prop 4], Cnj [Prop 5, Prop 6]]
  ]

main :: IO ()
main = do 
    putStrLn "=== Manual Example Tests ==="
    mapM_ (\(i, ex) -> do
        putStrLn $ "Testing Example " ++ show i ++ ": " ++ show (prop_cnf_equiv ex)
      ) (zip [(1::Int)..] manualExamples)

    putStrLn "\n=== QuickCheck CNF Equivalence Test ==="
    quickCheck prop_cnf_equiv
