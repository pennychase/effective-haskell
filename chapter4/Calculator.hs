module Calculator where

import Text.Read (readEither)

data Expr = Lit Int
          | Sub Expr Expr
          | Add Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

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

run :: String -> String
run expr =
    case parse expr of
        Left err -> "Error: " <> err
        Right expr' -> 
            let answer = show $ eval expr'
            in "The answer is: " <> answer
