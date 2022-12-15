module Fibs where

-- first version using map to generate infinit stream
fib' n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = (fib' $ n - 1) + (fib' $ n - 2)

fibs' = map fib' [0 ..]

smallFibs = takeWhile (<100) fibs'

-- first stream version
fibs'' firstFib secondFib =
    let nextFib = firstFib + secondFib
    in firstFib : fibs'' secondFib nextFib

-- second stream version that doesn't require input and recomputation
fibs = 0 : 1 : helper fibs (tail fibs)
    where
        helper (a:as) (b:bs) = a + b : helper as bs