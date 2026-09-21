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

--- Generate a random number, adjusted from previous exercises and lecture material
genSmallNat :: Int -> Int -> Gen Int
genSmallNat a b = chooseInt (a, b)

genRanVal :: [Valuation] -> Gen Valuation
genRanVal valuations = do
  k <- genSmallNat 0 ((length valuations)-1)
  return (valuations !! k)

checkEval :: Form -> Form -> Gen Bool
checkEval form cnf = do
  valuation <- genRanVal (genVals (propNames form))
  return (evl valuation form == evl valuation cnf)

checks :: [Form] -> Gen [Bool]
checks examples = mapM (\ex -> checkEval ex (cnf ex)) examples

main :: IO()
main = do 
    let example = (Equiv (Impl (Prop 1) (Prop 2)) (Dsj [Neg (Prop 3), Prop 4]))
    let example2 = (Dsj [Cnj [Prop 1, Prop 2], Cnj [Neg (Prop 3), Prop 4]])
    let example3 = (Dsj [Prop 1, Cnj [Prop 2, Dsj [Neg (Prop 3), Cnj[Prop 4, Prop 5]]]])
    let example4 = (Dsj [Dsj [Prop 1, Cnj [Prop 2, Neg (Prop 3)]], Cnj[Prop 4, Prop 5]])
    let example5 = (Dsj [Cnj[Prop 1, Neg (Prop 2)], Cnj[Neg (Prop 3), Prop 4], Cnj[Prop 5, Prop 6]])
    print (cnf example)
    print (cnf example2)
    print (cnf example3)
    print (cnf example4)
    print (cnf example5)
    checkList <- generate (checks [example, example2, example3, example4, example5])
    print checkList