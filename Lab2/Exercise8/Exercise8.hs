module Exercise8 where

import Data.Char
import Data.List
import Test.QuickCheck

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

---
---
--- Functions that implement the show functionality of a statement, recursively over statements, conditions, and expressions.
---
---

--- Helper for creating n spaces
indent :: Int -> String
indent n = replicate n ' '

--- Show Expression, using +, -, *
instance Show Expr where
    show (I int) = show int
    show (V var) = show var
    show (Add expr1 expr2) = "(" ++ show expr1 ++ " + " ++ show expr2 ++ ")"
    show (Subtr expr1 expr2) = "(" ++ show expr1 ++ " - " ++ show expr2 ++ ")"
    show (Mult expr1 expr2) = "(" ++ show expr1 ++ " * " ++ show expr2 ++ ")"

--- Show Conditions
--- shows symbols (==, <, >, ~, ^, v) rather than Eq, Lt, Gt, Ng, Cj, Dj
instance Show Condition where
    show (Prp var) = show var
    show (Eq expr1 expr2) = "(" ++ show expr1 ++ " == " ++ show expr2 ++ ")"
    show (Lt expr1 expr2) = "(" ++ show expr1 ++ " < " ++ show expr2 ++ ")"
    show (Gt expr1 expr2) = "(" ++ show expr1 ++ " > " ++ show expr2 ++ ")"
    show (Ng condition) = "(~" ++ show condition ++ ")"
    show (Cj []) = "true"
    show (Cj [condition]) = show condition
    show (Cj (condition : conditions)) =
        show condition ++ " ^ " ++ show (Cj conditions)
    show (Dj []) = "false"
    show (Dj [condition]) = show condition
    show (Dj (condition : conditions)) =
        show condition ++ " v " ++ show (Dj conditions)



--- Show statements, which requires indentation, for conditions and while statements.
--- Delimiters ; after assignments and {,} to indicate blocks make the output unambiguous for reading
--- Steps of 4 spaces are used for indentation
showIndentStatement :: Statement -> Int -> String
showIndentStatement (Ass var expr) n =
    indent n ++ show var ++ " <-- " ++ show expr ++ ";\n"
showIndentStatement (Seq statements) n = concatMap (`showIndentStatement` n) statements
showIndentStatement (Cond condition thenBranch elseBranch) n =
    indent n
        ++ "if " ++ show condition
        ++ " then {\n" ++ showIndentStatement thenBranch (n + 4)
        ++ indent n
        ++ "} else {\n" ++ showIndentStatement elseBranch (n + 4)
        ++ indent n ++ "}\n"
showIndentStatement (While condition body) n =
    indent n
        ++ "while (" ++ show condition ++ ") do {\n"
        ++ showIndentStatement body (n + 4)
        ++ indent n ++ "}\n"

instance Show Statement where
    show statement = showIndentStatement statement 0

---
---
--- Functions that implement the read functionality of statements,conditions, and expressions.
--- The implementation is based on the parser in Lecture3.hs
---
---

--- All possible tokens that can be read
data Token
    = TokenVar Var
    | TokenInt Integer
    | TokenAssign              --- <--
    | TokenPlus                --- +
    | TokenMinus               --- -
    | TokenMult                --- *
    | TokenEq                  --- ==
    | TokenLt                  --- <
    | TokenGt                  --- >
    | TokenNeg                 --- ~
    | TokenCnj                 --- ^
    | TokenDsj                 --- v
    | TokenIf
    | TokenThen
    | TokenElse
    | TokenWhile
    | TokenDo
    | TokenOP                  --- (
    | TokenCP                  --- )
    | TokenOB                  --- {
    | TokenCB                  --- }
    | TokenSemi                --- ;
    deriving (Show, Eq)

--- Lexer to read tokens from a string recursively
lexer :: String -> [Token]
lexer [] = []
lexer (c : cs)
    | isSpace c = lexer cs
    | isDigit c = lexInteger (c : cs)
    | isAlpha c || c == '_' = lexWord (c : cs)
lexer ('-' : c : cs)
    | isDigit c = lexInteger ('-' : c : cs)
lexer ('<' : '-' : '-' : cs) = TokenAssign : lexer cs
lexer ('=' : '=' : cs) = TokenEq : lexer cs
lexer ('+' : cs) = TokenPlus : lexer cs
lexer ('-' : cs) = TokenMinus : lexer cs
lexer ('*' : cs) = TokenMult : lexer cs
lexer ('<' : cs) = TokenLt : lexer cs
lexer ('>' : cs) = TokenGt : lexer cs
lexer ('~' : cs) = TokenNeg : lexer cs
lexer ('^' : cs) = TokenCnj : lexer cs
lexer ('(' : cs) = TokenOP : lexer cs
lexer (')' : cs) = TokenCP : lexer cs
lexer ('{' : cs) = TokenOB : lexer cs
lexer ('}' : cs) = TokenCB : lexer cs
lexer (';' : cs) = TokenSemi : lexer cs
lexer input@('"' : _) =
    case (reads input :: [(String, String)]) of
        [(var, rest)] -> TokenVar var : lexer rest
        _ -> error "Invalid quoted variable name."
lexer (x : _) = error ("Unknown token: " ++ [x])

--- Lexer for an integer that can be negative
lexInteger :: String -> [Token]
lexInteger ('-' : cs) =
    TokenInt (negate (read digits)) : lexer rest
    where
        (digits, rest) = span isDigit cs
lexInteger cs =
    TokenInt (read digits) : lexer rest
    where
        (digits, rest) = span isDigit cs

--- Word token assignment from a string
wordToken :: String -> Token
wordToken "if"    = TokenIf
wordToken "then"  = TokenThen
wordToken "else"  = TokenElse
wordToken "while" = TokenWhile
wordToken "do"    = TokenDo
wordToken "v"     = TokenDsj
wordToken var     = TokenVar var

--- Lexer for a word token
lexWord :: String -> [Token]
lexWord cs =
    wordToken word : lexer rest
    where
        (word, rest) = span (\char -> isAlphaNum char || char == '_') cs


--- A parser either fails ([]), or produces a parsed value together with the tokens that have not yet been consumed.
type Parser a b = [a] -> [(b, [a])]

--- Always succeeds without consuming any tokens. Used if empty and at the end of blocks, when consuming }
succeed :: b -> Parser a b
succeed value tokens = [(value, tokens)]

--- EXPRESSION PARSER

--- Parses an expression as an integer, variable, or connective of two expressions (+, -, or *)
parseExpr :: Parser Token Expr
parseExpr (TokenInt int : tokens) = [(I int, tokens)]
parseExpr (TokenVar var : tokens) = [(V var, tokens)]
parseExpr (TokenOP : tokens) =
    [ (Add expr1 expr2, rest)
    | (expr1, TokenPlus : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
    ++
    [ (Subtr expr1 expr2, rest)
    | (expr1, TokenMinus : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
    ++
    [ (Mult expr1 expr2, rest)
    | (expr1, TokenMult : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
parseExpr _ = []

--- CONDITION PARSING

--- Parse a lexed condition, starting with disjunction, and checking all options.
parseCondition :: Parser Token Condition
parseCondition = parseDisjunction

--- Parse a disjunction if exists, and continue searching for conjunction
parseDisjunction :: Parser Token Condition
parseDisjunction tokens =
    [ (makeDj condition1 condition2, rest)
    | (condition1, TokenDsj : remaining) <- parseConjunction tokens,
    (condition2, rest) <- parseDisjunction remaining
    ]
    ++ parseConjunction tokens

--- Parse a conjunction if exists, and continue searching for atomic conditions
parseConjunction :: Parser Token Condition
parseConjunction tokens =
    [ (makeCj condition1 condition2, rest)
    | (condition1, TokenCnj : remaining) <- parseConditionAtom tokens,
    (condition2, rest) <- parseConjunction remaining
    ]
    ++ parseConditionAtom tokens

--- Parse a proposition, negation (~condition), comparison (==, <, >) or further condition in parantheses, which can be the case for Cnj/Dsj.
parseConditionAtom :: Parser Token Condition
parseConditionAtom (TokenVar var : tokens) = [(Prp var, tokens)]
parseConditionAtom (TokenOP : TokenNeg : tokens) =
    [ (Ng condition, rest)
    | (condition, TokenCP : rest) <- parseCondition tokens
    ]
parseConditionAtom (TokenOP : tokens) =
    [ (condition, rest)
    | (condition, TokenCP : rest) <- parseCondition tokens
    ]
    ++
    [ (Eq expr1 expr2, rest)
    | (expr1, TokenEq : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
    ++
    [ (Lt expr1 expr2, rest)
    | (expr1, TokenLt : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
    ++
    [ (Gt expr1 expr2, rest)
    | (expr1, TokenGt : remaining) <- parseExpr tokens,
    (expr2, TokenCP : rest) <- parseExpr remaining
    ]
parseConditionAtom _ = []

--- Combine two conditions into conjunction. Used to avoid nested brackets.
makeCj :: Condition -> Condition -> Condition
makeCj condition1 (Cj conditions) = Cj (condition1 : conditions)
makeCj condition1 condition2 = Cj [condition1, condition2]

--- Combine two conditions into disjunction, identically to makeCj
makeDj :: Condition -> Condition -> Condition
makeDj condition1 (Dj conditions) = Dj (condition1 : conditions)
makeDj condition1 condition2 = Dj [condition1, condition2]

--- STATEMENT PARSING

--- Parse statement, which can either be an assignment, a conditional block or a while block. 
--- Note that sequences have been filtered out by makeSeq.
parseStatement :: Parser Token Statement
parseStatement tokens = parseAssignment tokens ++ parseIf tokens ++ parseWhile tokens

--- Parse assignment of form var <-- value;
parseAssignment :: Parser Token Statement
parseAssignment (TokenVar var : TokenAssign : tokens) =
    [ (Ass var expr, rest)
    | (expr, TokenSemi : rest) <- parseExpr tokens
    ]
parseAssignment _ = []

--- Parse if block, including if statement, condition, then and associated block in {}, and else with associated block in {}
parseIf :: Parser Token Statement
parseIf (TokenIf : tokens) =
    [ (Cond condition thenStatement elseStatement, rest)
    | (condition, TokenThen : afterCondition) <- parseCondition tokens,
    (thenStatement, TokenElse : afterThen) <- parseBlock afterCondition,
    (elseStatement, rest) <- parseBlock afterThen
    ]
parseIf _ = []

--- Parse while block, including the while statement, condition including (), and body in {} using parseBlock
parseWhile :: Parser Token Statement
parseWhile (TokenWhile : TokenOP : tokens) =
    [ (While condition body, rest)
    | (condition, TokenCP : TokenDo : afterCondition) <- parseCondition tokens,
    (body, rest) <- parseBlock afterCondition
    ]
parseWhile _ = []


--- Parse a block of statements contained within {,} braces
parseBlock :: Parser Token Statement
parseBlock (TokenOB : tokens) =
    [ (makeSeq statements, rest)
    | (statements, TokenCB : rest) <- parseStatementList tokens
    ]
parseBlock _ = []

--- Split list of statements, parse individually, and recombine. Ends at }, to separately handle each block using parseBlock
parseStatementList :: Parser Token [Statement]
parseStatementList [] = succeed [] []
parseStatementList tokens@(TokenCB : _) = succeed [] tokens
parseStatementList tokens =
    [ (statement : statements, rest)
    | (statement, remaining) <- parseStatement tokens,
    (statements, rest) <- parseStatementList remaining
    ]


--- Create a sequence from a list of statements
makeSeq :: [Statement] -> Statement
makeSeq [] = Seq []
makeSeq [statement] = statement
makeSeq statements = Seq statements


--- Initial read call, Parses a complete set of statements with output from the lexer
--- Return [] if invalid input
readStatement :: String -> [Statement]
readStatement input =
    [ makeSeq statements
    | (statements, []) <- parseStatementList (lexer input),
    not (null statements)
    ]

--- TESTING

--- Generates variable names from selection in working example
genVar :: Gen Var
genVar =
    elements
        [ "x",
        "y",
        "total",
        "limit",
        "counter"
        ]

--- Generates small integers
genInteger :: Gen Integer
genInteger = choose (-20, 20)

--- Generates either an integer expression or a variable expression
genAtomicExpr :: Gen Expr
genAtomicExpr =
    oneof [ I <$> genInteger, V <$> genVar]

--- Generates one simple arithmetic expression
genExpr :: Gen Expr
genExpr = do
    operator <- elements [Add, Subtr, Mult]
    expr1 <- genAtomicExpr
    expr2 <- genAtomicExpr
    return (operator expr1 expr2)

--- Generates one simple comparison condition
genComparison :: Gen Condition
genComparison = do
    operator <- elements [Eq, Lt, Gt]
    expr1 <- genExpr
    expr2 <- genExpr
    return (operator expr1 expr2)

--- Generates one compound condition using either ^ or v and testing negation
genCondition :: Gen Condition
genCondition = do
    connective <- elements [Cj, Dj]
    condition1 <- genComparison
    condition2 <- Ng <$> genComparison
    return (connective [condition1, condition2])

--- Generates one statement containing assignments, a conditional, a while-loop, and a sequence, based on the working example used
genStatement :: Gen Statement
genStatement = do
    var1 <- genVar
    var2 <- genVar

    expr1 <- genExpr
    expr2 <- genExpr

    ifCondition <- genCondition
    whileCondition <- genCondition

    return
        (Seq
        [ Ass var1 expr1

        , Cond
            ifCondition
            (Ass var1 (Add (V var1) (I 1)))
            (Ass var1 (Subtr (V var1) (I 1)))

        , While
            whileCondition
            (Seq
                [ Ass var1 (Add (V var1) expr2)
                , Ass var2 (Subtr (V var2) (I 1))
                ])
        ])

--- Property to test whether readStatement and show are correct inverses of each other
prop_readShowStatement :: Property
prop_readShowStatement =
    forAll genStatement $ \statement ->
        counterexample
        ("Statement text:\n" ++ show statement)
        (readStatement (show statement) === [statement])

--- Reverse property, avoiding generating printed form statements
prop_showReadStatement :: Property
prop_showReadStatement =
    forAll genStatement $ \statement ->
        counterexample
        ("Statement text:\n" ++ show statement ++ "\nshow read statement:\n" ++ show (readStatement (show statement)))
        (show (readStatement (show statement)) === show ([statement]))

main :: IO ()
main = do
    let complexStatement =
            Seq
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

        complexStatementText =
            unlines
                [ "\"total\" <-- 0; \"limit\" <-- (10 + (2 * 5)); while ((\"total\" < \"limit\") ^ (~(\"total\" == 13))) do {"
                , "if ((\"total\" == 0) v (\"total\" > 15)) then {"
                , "\"total\" <-- (\"total\" + 2);"
                , "} else {"
                , "\"total\" <-- (\"total\" + 1);"
                , "}"
                , "if (\"total\" > 17) then {"
                , "\"limit\" <-- (\"limit\" - 3);"
                , "} else {"
                , "\"limit\" <-- (\"limit\" + 1);"
                , "}"
                , "}"
                ]

    -- print complexStatement
    -- print (readStatement complexStatementText)
    quickCheck prop_readShowStatement
    quickCheck prop_showReadStatement
