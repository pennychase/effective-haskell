module Calculator
    ( Operator (..)
    , calculate
    , toOperator
    ) where

data Operator = Add | Sub | Mul | Div

toOperator :: String -> Maybe Operator
toOperator str =
    case str of
        "+" -> Just Add
        "-" -> Just Sub
        "*" -> Just Mul
        "/" -> Just Div
        _ -> Nothing

calculate :: Operator -> (Int, Int) -> Int
calculate op (arg1, arg2) =
    case op of
        Add -> arg1 + arg2
        Sub -> arg1 - arg2
        Mul -> arg1 * arg2
        Div -> arg1 `div` arg2
