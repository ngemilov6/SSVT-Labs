module Exercise8 where

type Var = String

type Env = Var -> Integer

data Expr
  = I Integer
  | V Var
  | Add Expr Expr
  | Subtr Expr Expr
  | Mult Expr Expr
  deriving (Eq)

data Condition
  = Prp Var
  | Eq Expr Expr
  | Lt Expr Expr
  | Gt Expr Expr
  | Ng Condition
  | Cj [Condition]
  | Dj [Condition]
  deriving (Eq)

data Statement
  = Ass Var Expr
  | Cond Condition Statement Statement
  | Seq [Statement]
  | While Condition Statement
  deriving (Eq)

-- Helper for creating n spaces
indent :: Int -> String
indent n = replicate n ' '

--- Expressions don't need indentation
showExpr :: Expr -> String
showExpr (I int) = show int
showExpr (V var) = show var
showExpr (Add expr1 expr2) =
  "(" ++ showExpr expr1 ++ " + " ++ showExpr expr2 ++ ")"
showExpr (Subtr expr1 expr2) =
  "(" ++ showExpr expr1 ++ " - " ++ showExpr expr2 ++ ")"
showExpr (Mult expr1 expr2) =
  "(" ++ showExpr expr1 ++ " * " ++ showExpr expr2 ++ ")"

instance Show Expr where
  show expr = showExpr expr

--- Show Conditions, also no indentation necessary
--- shows symbols (==, <, >, ~, ^, v) rather than Eq, Lt, Gt, Ng, Cj, Dj
showCondition :: Condition -> String
showCondition (Prp var) =
  show var

showCondition (Eq expr1 expr2) =
  "(" ++ showExpr expr1 ++ " == " ++ showExpr expr2 ++ ")"

showCondition (Lt expr1 expr2) =
  "(" ++ showExpr expr1 ++ " < " ++ showExpr expr2 ++ ")"

showCondition (Gt expr1 expr2) =
  "(" ++ showExpr expr1 ++ " > " ++ showExpr expr2 ++ ")"

showCondition (Ng condition) =
  "(~" ++ showCondition condition ++ ")"

showCondition (Cj []) =
  "true"

showCondition (Cj [condition]) =
  showCondition condition

showCondition (Cj (condition : conditions)) =
  showCondition condition
    ++ " ^ "
    ++ showCondition (Cj conditions)

showCondition (Dj []) =
  "false"

showCondition (Dj [condition]) =
  showCondition condition

showCondition (Dj (condition : conditions)) =
  showCondition condition
    ++ " v "
    ++ showCondition (Dj conditions)

instance Show Condition where
  show condition = showCondition condition


--- Show statements, which requires indentation, for conditions and while statements.
--- Delimiters ; after assignments and {,} to indicate blocks make the output unambiguous for reading
--- Steps of 4 spaces are used for indentation
showIndentStatement :: Statement -> Int -> String
showIndentStatement (Ass var expr) n =
  indent n
    ++ show var
    ++ " <-- "
    ++ showExpr expr
    ++ ";\n"

showIndentStatement (Seq statements) n =
  concatMap (`showIndentStatement` n) statements

showIndentStatement (Cond condition thenBranch elseBranch) n =
  indent n
    ++ "if "
    ++ showCondition condition
    ++ " then {\n"
    ++ showIndentStatement thenBranch (n + 4)
    ++ indent n
    ++ "} else {\n"
    ++ showIndentStatement elseBranch (n + 4)
    ++ indent n
    ++ "}\n"

showIndentStatement (While condition body) n =
  indent n
    ++ "while ("
    ++ showCondition condition
    ++ ") do {\n"
    ++ showIndentStatement body (n + 4)
    ++ indent n
    ++ "}\n"
-- showIndentStatement :: Statement -> Int -> String
-- showIndentStatement (Ass var expr) n =
--   indent n
--     ++ show var
--     ++ " <-- "
--     ++ showExpr expr
--     ++ "\n"

-- showIndentStatement (Cond condition s1 s2) n =
--   indent n
--     ++ "if "
--     ++ showCondition condition
--     ++ " then\n"
--     ++ showIndentStatement s1 (n + 4)
--     ++ indent n
--     ++ "else\n"
--     ++ showIndentStatement s2 (n + 4)

-- showIndentStatement (Seq statements) n =
--   concatMap (`showIndentStatement` n) statements

-- showIndentStatement (While condition statement) n =
--   indent n
--     ++ "while ("
--     ++ showCondition condition
--     ++ ") do\n"
--     ++ showIndentStatement statement (n + 4)

instance Show Statement where
  show statement = showIndentStatement statement 0

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