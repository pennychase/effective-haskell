module Exercises1 where

factorial :: Int -> Int
factorial n =
    if n == 1
        then 1
        else n * factorial (n - 1)

naiveFibonacci n =
    if n <= 1 
        then 1
        else naiveFibonacci (n - 1) + naiveFibonacci (n - 2)

fibonacci :: Integer -> Integer
fibonacci n = 
    fib n 1 1
    where
        fib n a b =
            if n <= 1
                then b
                else fib (n - 1) b (a + b)

curry' f x y = f (x, y)

uncurry' f (x, y) = f x y