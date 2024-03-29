module Calculator where

import Text.Read (readEither)

data Expr = Lit Int
          | Sub Expr Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
        deriving Show

-- evaluate Expr
eval :: Expr -> Int
eval expr =
    case expr of
        Lit num -> num
        Sub arg1 arg2 -> eval' (-) arg1 arg2
        Add arg1 arg2 -> eval' (+) arg1 arg2
        Mul arg1 arg2 -> eval' (*) arg1 arg2
        Div arg1 arg2 -> eval' div arg1 arg2
    where
        eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Int
        eval' operator arg1 arg2 = operator (eval arg1) (eval arg2)

-- Parse a string with arithmetic operators in prefix notation
parse :: String -> Either String Expr
parse str =
    case parse' (words str) of
        Left err -> Left err
        Right (e, []) -> Right e
        Right (_, rest) -> Left $ "Found extra tokens: " <> (unwords rest)

parse' :: [String] -> Either String (Expr, [String])
parse' [] = Left "unexpcted end of expression"
parse' (token:rest) =
    case token of
        "-" -> parseBinary Sub rest
        "+" -> parseBinary Add rest
        "*" -> parseBinary Mul rest
        "/" -> parseBinary Div rest
        lit ->
            case readEither lit of
                Left err -> Left err
                Right lit' -> Right (Lit lit', rest)

parseBinary :: (Expr -> Expr -> Expr) -> [String] -> Either String (Expr, [String])
parseBinary exprConstructor args =
    case parse' args of
        Left err -> Left err
        Right (firstArg, rest') ->
            case parse' rest' of
                Left err -> Left err
                Right (secondArg, rest'') -> Right $ (exprConstructor firstArg secondArg, rest'')

-- Parse a string and evaluate
run :: String -> String
run expr =
    case parse expr of
        Left err -> "Error: " <> err
        Right expr' -> 
            let answer = show $ eval expr'
            in "The answer is: " <> answer


-- Exercise: Write safeEval that returns error of division by 0
safeEval :: Expr -> Either String Int
safeEval expr =
    case expr of
        Lit num ->  Right num
        Sub arg1 arg2 ->  eval' (-) arg1 arg2
        Add arg1 arg2 ->  eval' (+) arg1 arg2
        Mul arg1 arg2 ->  eval' (*) arg1 arg2
        Div arg1 arg2 -> 
            case safeEval arg2 of
                Right 0 -> Left "Division by zero"
                _ -> eval' div arg1 arg2
    where
        eval' :: (Int -> Int -> Int) -> Expr -> Expr -> Either String Int
        eval' operator arg1 arg2 = Right $ operator (eval arg1) (eval arg2)

safeRun :: String -> String
safeRun expr =
    case parse expr of
        Left err -> "Error: " <> err
        Right expr' -> 
            case  safeEval expr' of
                Left err' -> "Error: " <> err'
                Right answer' -> "The answer is: " <> show answer'

-- Exercise: Pretty printer for Expr
-- Prettyprint the Expr and result of evaluation
prettyPrint :: Expr -> String
prettyPrint expr = prettyPrint' expr <> " = " <> (show $ eval expr)

-- Print an Expr with infix operators and parentheses
prettyPrint' :: Expr -> String
prettyPrint' expr =
    case expr of
        Lit num -> show num
        Sub arg1 arg2 -> prettyPrint'' " - " arg1 arg2
        Add arg1 arg2 -> prettyPrint'' " + " arg1 arg2
        Mul arg1 arg2 -> prettyPrint'' " * " arg1 arg2
        Div arg1 arg2 -> prettyPrint'' " / " arg1 arg2
    where
        prettyPrint'' :: String -> Expr -> Expr -> String
        prettyPrint'' operator arg1 arg2 =
            case  arg2 of
                Lit _ -> prettyPrint' arg1 <> operator <> prettyPrint' arg2 
                _ -> prettyPrint' arg1 <> operator <> "( " <> prettyPrint' arg2 <> " )"
     