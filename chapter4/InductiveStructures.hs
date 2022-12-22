module InductiveStructures where

-- Peano Numbers

data Peano = Z | S Peano
    deriving Show

toPeano :: Int -> Peano 
toPeano 0 = Z
toPeano n = S (toPeano $ n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ (fromPeano $ p)

eqPeano :: Peano -> Peano -> Bool
eqPeano p p' =
    case (p,p') of
        (Z,Z) -> True
        (S n, S n') -> eqPeano n n'
        _ -> False

addPeano :: Peano -> Peano -> Peano
addPeano Z p = p
addPeano (S p) p' = addPeano p (S p')

-- Lists

data List a = Empty | Cons a (List a)
        deriving Show

toList :: [a] -> List a
toList [] = Empty
toList (x:xs) = Cons x (toList xs)

fromList :: List a -> [a]
fromList Empty = []
fromList (Cons x xs) = x : fromList xs

toList' :: [a] -> List a
toList' = foldr Cons Empty

fromList' :: List a -> [a]
fromList' = listFoldr (:) []

listFoldr :: (a -> b -> b) -> b -> List a -> b
listFoldr _ b Empty = b
listFoldr f b (Cons x xs) = f x (listFoldr f b xs)

listFoldl :: (b -> a -> b) -> b -> List a -> b
listFoldl _ b Empty = b
listFoldl f b (Cons x xs) = listFoldl f (f b x) xs

listHead :: List a -> Maybe a
listHead xs =
    case xs of
        Empty -> Nothing
        Cons x _ -> Just x

listTail :: List a -> List a
listTail xs =
    case xs of
        Empty -> Empty
        Cons _ ys -> ys

listReverse :: List a -> List a
listReverse = listFoldl (flip Cons) Empty

listMap :: (a -> b) -> List a -> List b
listMap f = listFoldr (\x xs -> Cons (f x) xs) Empty


