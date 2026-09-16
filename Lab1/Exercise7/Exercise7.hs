import Lecture3
import Data.Bits (Bits(xor))
import Prelude 
import Test.QuickCheck

cnf :: Form -> Form
cnf = toCNF . nnf . arrowfree 

toCNF :: Form -> Form
toCNF (Cnj fs) = Cnj (mergeCnj (map toCNF fs))
toCNF (Dsj []) = Dsj []
toCNF (Dsj (f:fs)) = foldl dist (toCNF f) (map toCNF fs)
toCNF f = f

mergeCnj :: [Form] -> [Form]
mergeCnj [] = []
mergeCnj (x:xs) = case x of
  Cnj forms -> forms ++ mergeCnj xs
  form -> [form] ++ mergeCnj xs

dist :: Form -> Form -> Form
dist (Cnj fs1) (Cnj fs2) = Cnj [dist f1 f2 | f1<- fs1, f2<-fs2]
dist (Cnj fs1) f2 = Cnj [dist f1 f2 | f1 <- fs1]
dist f1 (Cnj fs2) = Cnj [dist f1 f2 | f2 <- fs2]
dist (Dsj fs1) (Dsj fs2) = Dsj (fs1 ++ fs2)
dist (Dsj fs1) f2 = Dsj (fs1 ++ [f2])
dist f1 (Dsj fs2) = Dsj (f1 : fs2)
dist f1 f2 = Dsj [f1, f2]


instance Arbitrary Form where
  arbitrary = sized (\n -> genForm (min n 6)) -- Cap the max depth parameter
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

prop_cnf_equiv :: Form -> Bool
prop_cnf_equiv f = all (\v -> evl v f == evl v f') (allVals f) -- Evaluates against the smaller original tree
  where f' = cnf f


main :: IO ()
main = do 
    putStrLn "=== Property Tests ==="
    quickCheck prop_cnf_equiv