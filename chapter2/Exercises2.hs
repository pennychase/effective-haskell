module Exercises2 where

-- Exercise 1
-- Write reverse using folds
-- Using foldr should be more expensive since appending creates new lists.
-- Timing (using :set +s)
-- reverseR [1..10000] -> 0.95 secs, 4,333,514,912 bytes
-- reverseL [1..10000] -> 0.23 secs, 41,104,288 bytes
reverseR :: [a] -> [a]
reverseR xs = foldr (\x ys -> ys <> [x]) [] xs

reverseL :: [a] -> [a]
reverseL xs = foldl (flip (:)) [] xs

-- Exercise 2

-- Recursive
zipWith' f [] ys = []
zipWith' f xs []  = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys

-- List Comprehension (need to use or define zip)
zipWith'' f xs ys = [ x + y | (x, y) <- zip xs ys]

-- foldr
zipWith''' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith''' f xs ys = foldr (\z zs -> uncurry f z : zs) [] (zip xs ys)

-- Exercise 3
-- concatMap is defined as concat . map
-- This uses foldr, the first is concat and the second is map
concatMap' f xs = foldr (<>) [] (foldr (\y ys -> f y : ys) [] xs)
