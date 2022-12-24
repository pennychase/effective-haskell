module HaskellBook.Examples.SortedList
    ( SortedList (getSorted)
    , makeSortedList
    , minimum
    , testMinimum
    ) where

import Data.List (sort)
import Prelude hiding (minimum)

-- Smart Constructors

data SortedList = SortedList { getSorted :: [Int] }

makeSortedList :: [Int] -> Maybe SortedList
makeSortedList [] = Nothing
makeSortedList nums = Just $ SortedList (sort nums)

minimum :: SortedList -> Int
minimum (SortedList nums) = head nums

-- Example of using Smart Constructor
testMinimum :: [Int] -> String
testMinimum input =
    case lis of
        Nothing -> "Illegal list"
        Just lis' -> "Minimum is " <> show (minimum lis')
    where
        lis = makeSortedList input

-- Can also test using Functor fmap:
--    minimum <$> makeSortedList [5,7,4,9,2,8,6] => Just 2
--    minimum <$> makeSortedList [] => Nothing

