module Exercise8 where

-- import Lecture4

type Var = String

type Env = Var -> Integer

data Expr = I Integer | V Var 
          | Add Expr Expr 
          | Subtr Expr Expr 
          | Mult Expr Expr 
          deriving (Eq)

instance Show Expr where
    show (I int) = show int
    show (V var) = show var
    show (Add expr1 expr2) = "(" ++ show expr1 ++ " + " ++ show expr2 ++ ")"
    show (Subtr expr1 expr2) = "("++show expr1 ++ " - " ++ show expr2 ++ ")"
    show (Mult expr1 expr2) = "("++show expr1 ++ " * " ++ show expr2 ++ ")"

data Condition = Prp Var 
               | Eq Expr Expr 
               | Lt Expr Expr 
               | Gt Expr Expr 
               | Ng Condition 
               | Cj [Condition] 
               | Dj [Condition]
               deriving (Eq)

instance Show Condition where
    show (Prp var) = show var
    show (Eq expr1 expr2) = "(" ++ show expr1 ++ " == " ++ show expr2 ++ ")"
    show (Lt expr1 expr2) = "(" ++ show expr1 ++ " < " ++ show expr2 ++ ")"
    show (Gt expr1 expr2) = "(" ++ show expr1 ++ " > " ++ show expr2 ++ ")"
    show (Ng expr) = "( ~" ++ show expr ++ ")"
    show (Cj (cond:[])) = show cond
    show (Cj (cond:conds)) = show cond ++ " ^ " ++ show (Cj conds)
    show (Dj (cond:[])) = show cond
    show (Dj (cond:conds)) = show cond ++ " v " ++ show (Dj conds)

showIndent :: Statement -> Int -> String
showIndent (Ass var expr) n = (repeat n " ") ++ showIndent var n ++ " <-- " ++ showIndent expr n ++ "\n"
showIndent (Cond condition s1 s2) n = (repeat n " ") + "if " ++ showIndent condition 0 ++ "\n" ++ (repeat (n+4) " ") ++ "then \n" ++ showIndent s1 (n+8) ++ (repeat (n+4) " ") ++"else " ++ showIndent s2 (n+8)
showIndent (Seq ss) n = (repeat n " ") ++ showLst ss
showIndent (While condition s1) n = (repeat n " ") ++ "while (" ++ showIndent condition 0 ++ ")\n" ++ (repeat n " ") ++ showIndent s1 (n+4)


data Statement = Ass Var Expr
               | Cond Condition Statement Statement
               | Seq [Statement]
               | While Condition Statement
               deriving (Eq)

instance Show Statement where
  show (Ass var expr) = showIndent (Ass var expr) 0
  show (Cond condition s1 s2) = showIndent (Cond condition s1 s2) 0
  show (Seq ss) = showIndent (Seq ss) 0
  show (While condition s1) = showIndent (While condition s1) 0
--   show (Ass var expr) = show var ++ " <-- " ++ show expr ++ "\n"
--   show (Cond condition s1 s2) = "if " ++ show condition ++ "\n     then " ++ show s1 ++ "     else " ++ show s2
--   show (Seq ss) = showLst ss
--   show (While condition s1) = "while (" ++ show condition ++ ")\n   " ++ show s1

showLst, showRest :: [Statement] -> String
showLst [] = ""
showLst (f : fs) = show f ++ showRest fs
showRest [] = ""
showRest (f : fs) = ' ' : show f ++ showRest fs

main :: IO ()
main = do
  let complexStatement = Seq
        [ Ass "total" (I 0)
        , Ass "limit" (Add (I 10) (Mult (I 2) (I 5)))
        , While
            (Cj
              [ Lt (V "total") (V "limit")
              , Ng (Eq (V "total") (I 13))
              ])
            (Seq
              [ Cond
                  (Dj
                    [ Eq (V "total") (I 0)
                    , Gt (V "total") (I 15)
                    ])
                  (Ass "total" (Add (V "total") (I 2)))
                  (Ass "total" (Add (V "total") (I 1)))
              , Cond
                  (Gt (V "total") (I 17))
                  (Ass "limit" (Subtr (V "limit") (I 3)))
                  (Ass "limit" (Add (V "limit") (I 1)))
              ])
        ]

  print complexStatement