module FizzBuzz where

import Data.List as List

fizzBuzzFor :: Int -> String
fizzBuzzFor number
    | 0 == number `rem` 15  = "fizzbuzz"
    | 0 == number `rem` 3   = "fizz"
    | 0 == number `rem` 5   = "buzz"
    | otherwise = show number

fizzBuzz :: Int -> String
fizzBuzz fizzBuzzNumber =
--    foldr (\n ns -> fizzBuzzFor n <> " " <> ns) "" [1 .. fizzBuzzNumber]
    List.intercalate " " $ map fizzBuzzFor [1 .. fizzBuzzNumber]

naiveFizzBuzz :: Int -> Int -> String -> String
naiveFizzBuzz fizzBuzzCount curNum fizzBuzzString =
    if curNum > fizzBuzzCount
        then fizzBuzzString
        else 
            let nextFizzBuzzString = fizzBuzzString <> fizzBuzzFor curNum <> " "
                nextNum = curNum + 1
            in naiveFizzBuzz fizzBuzzCount nextNum nextFizzBuzzString