module OrdExample where

import Prelude hiding (Ord(..), Ordering(..))
import Data.Word (Word8)

data Ordering = LT | EQ | GT
        deriving Show

-- Ord class definition provides defaults for all the functions in terms of compare.
-- Compare is also defined in terms of (<=) so that a minimal defintion can define compare or (<=)
class Eq a => Ord a where
    compare :: a -> a -> Ordering
    compare a b 
        | a == b = EQ
        | a <= b = LT
        | otherwise = GT
    (<) :: a -> a -> Bool
    a < b =
        case compare a b of
            LT -> True
            _ -> False
    (<=) :: a -> a -> Bool
    a <= b =
        case compare a b of
            GT -> False
            _ -> True
    (>) :: a -> a -> Bool
    a > b =
        case compare a b of
            GT -> True
            _ -> False
    (>=) :: a -> a -> Bool
    a >= b =
        case compare a b of
            LT -> False
            _ -> True
    max :: a -> a -> a
    max a b =
        case compare a b of
            GT -> a
            _ -> b
    min :: a -> a -> a
    min a b =
        case compare a b of
            LT -> a
            _ -> b
    {-# MINIMAL compare | (<=) #-}

-- Instance for Word8
-- Shows implmentation using compare and (commented out) using (<=)
instance Ord Word8 where
    compare a b
        | a == b = EQ
        | a == 0 = LT
        | b == 0 = GT
        | otherwise = compare (a - 1) (b - 1)

    -- a <= b
    --     | a == b = True
    --     | a == 0 = True
    --     | b == 0 = False
    --     | otherwise = (a - 1) <= (b - 1)


