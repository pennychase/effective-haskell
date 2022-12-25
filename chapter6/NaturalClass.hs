module NaturalClass where

class (Show n, Eq n) => Natural n where
    add :: n -> n -> n
    multiply :: n -> n -> n
    additiveIdentity :: n
    multiplicativeIdentity :: n

-- Int instance 
instance Natural Int where
    add = (+)
    multiply = (*)
    additiveIdentity = 0
    multiplicativeIdentity = 1

-- Peano instance

-- data type and some conversion helpers
data Peano = Z | S Peano

toPeano :: Int -> Peano 
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ $ fromPeano  p

-- Eq instance
instance Eq Peano where
    (==) Z Z = True
    (==) (S a) (S b) = a == b
    (==) _ _ = False

-- Show instance
instance Show Peano where
    show Z = "Z"
    show (S a) = "(S " <> show a <> ")"

instance Natural Peano where
    add Z b = b
    add (S a) b = add a (S b)

    multiply Z _ = Z
    multiply (S a) b = add b (multiply a b)

    additiveIdentity = Z
    multiplicativeIdentity = S Z

    


unique :: Natural a => [a] -> [a]
unique [] = []
unique (elem:elems) =
    let
        compare a b = not $ a == b elems
    in elem : unique elems