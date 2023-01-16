{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.String

-- My Types (type definitions and helper functions)

-- List
data List a = Empty | List a (List a)

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = List x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (List x xs) = x : fromList xs

concatList :: List a -> List a -> List a
concatList Empty xs = xs
concatList (List x xs) ys = List x (concatList xs ys)

instance Show a => Show (List a) where
    show = show . fromList


-- Function
newtype Function a b = Function 
    { runFunction :: a -> b }

-- Functors

instance Functor List where
    fmap _ Empty = Empty
    fmap f (List x xs) = List (f x) (fmap f xs) 

instance Functor (Function a) where
    fmap f (Function g) = Function (f . g)


-- Applicatives

instance Applicative List where
    pure a = List a Empty

    Empty <*> _ = Empty
    List f fs <*> vals = (f <$> vals) `concatList` (fs <*> vals)

instance Applicative (Function a) where
    pure a = Function $ const a
    Function f <*> Function g = Function $ \value -> f value (g value)


-- Monads

instance Monad List where
    return = pure
    Empty >>= f = Empty
    List a as >>= f = (f a) `concatList` (as >>= f)

-- Examples

-- Function as Functor

addOne = Function { runFunction = (+1) }

myShow = Function { runFunction = show @Integer }

-- (runFunction myShow) <$> runFunction addOne $ 1 
-- >>> "2"

-- List as Applicative
-- fromList $ toList [id, (+1), (*2)] <*> (toList [1..4])
-- >>> [1,2,3,4,2,3,4,5,2,4,6,8]
-- pure (+1) <*> (toList [1..4])
-- >>> List 2 (List 3 (List 4 (List 5 Empty)))

-- Function as Applicative
myMult = Function { runFunction = (*) }

-- runFunction (myMult <*> addOne) 3
-- >>> 12

-- List as Monad

instance IsString (List Char) where
    fromString = toList

type StringL = List Char

replicateL :: Int -> a -> List a
replicateL 0 _ = Empty
replicateL n x = List x (replicateL (n - 1) x)

wordsL :: StringL -> List StringL
wordsL = toList . map toList . words . fromList

unwordsL :: List StringL -> StringL
unwordsL = toList . unwords . map fromList . fromList
