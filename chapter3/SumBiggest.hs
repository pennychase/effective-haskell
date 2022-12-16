module SumBiggest where

import qualified Data.List

{-
-- This is the first version, demonstrating typechecking works even if funciton is undefined

sumBiggest :: [[Int]] ->String
sumBiggest allNums = undefined
-}

sumBiggest :: [[Int]] -> String
sumBiggest allNums = 
    let
        getBiggest :: [Int] -> [Int]
        getBiggest nums = 
            let
                biggest = maximum nums
            in filter (==biggest) nums

        getSmallest :: [Int] -> [Int]
        getSmallest nums = 
            let 
                smallest = minimum nums
            in filter (==smallest) nums

        differences :: ([Int], [Int]) -> Int
        differences (biggests, smallests) = sum biggests - sum smallests

        allBiggests :: [[Int]]
        allBiggests = map getBiggest allNums

        allSmallests :: [[Int]]
        allSmallests = map getSmallest allNums

        sizePairs :: [([Int],[Int])]
        sizePairs = zip allBiggests allSmallests

        differences' :: [String]
        differences' = map (show . differences) sizePairs
    in Data.List.intercalate "," differences'

showBiggest =
    let biggestInfo = sumBiggest [[1,1,2,3,4,4],[1,2,5,5],[-1,-2,5,-10,5]]
    in print $ "sumBiggest says: " <> biggestInfo

