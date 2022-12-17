module Exercises3 where

-- Undefined
-- We can write addThree in many ways with undefined as we incremently develop our code. This enables 
-- us to typecheck partial implementations
addThree :: Int -> Int -> Int -> Int
addThree = undefined

addThree' :: Int -> Int -> Int -> Int
addThree' a b c = 
    let
        s = a + b
    in undefined

-- Understand functions by their type
swap' :: (a,b) -> (b,a)
swap' (a,b) = (b,a)

concat' :: [[a]] -> [a]
concat' = foldr (<>) []

id' :: a -> a
id' a = a

-- Filing in type holes

mapApply :: [a -> b] -> [a] -> [b]
mapApply toApply = concatMap (\input -> map ($ input) toApply)

example :: [Int] -> String
example nums = map lookupLetter $ mapApply offsets nums
    where
        letters :: [Char]
        letters = ['a' .. 'z']

        lookupLetter :: Int -> Char
        lookupLetter n = letters !! n

        offsets :: [Int -> Int]
        offsets = [rot13, swap10, mixupVowels]

        rot13 :: Int -> Int
        rot13 n = (n + 13) `rem` 26

        swap10 :: Int -> Int
        swap10 n
            | n <= 10 = n + 10
            | n <= 20 = n - 10
            | otherwise = n

        mixupVowels :: Int -> Int
        mixupVowels n =
            case n of
                0 -> 8      -- a -> i
                4 -> 14     -- e -> o
                8 -> 20     -- i -> u
                14 -> 0     -- o -> a
                20 -> 4     -- u -> e
                n' -> n'
