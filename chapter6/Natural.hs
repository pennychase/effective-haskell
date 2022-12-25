module Natural where

data Peano = Z | S Peano

toPeano :: Int -> Peano 
toPeano 0 = Z
toPeano n = S $ toPeano (n - 1)

fromPeano :: Peano -> Int
fromPeano Z = 0
fromPeano (S p) = succ $ fromPeano  p

eqPeano Z Z = True
eqPeano (S a) (S b) = eqPeano a b
eqPeano _ _ = False

addPeano Z b = b
addPeano (S a) b = addPeano a (S b)

multiplyPeano Z _ = Z
multiplyPeano (S a) b = addPeano b (multiplyPeano a b)

data Natural a = Natural
    { equal :: a -> a -> Bool
    , add :: a -> a -> a
    , multiply :: a -> a -> a
    , additiveIdentity :: a
    , multiplicativeIdentity :: a
    , displayAsString :: a -> String
    }

intNatural :: Natural Int
intNatural = Natural
    { equal = (==)
    , add = (+)
    , multiply = (*)
    , additiveIdentity = 0
    , multiplicativeIdentity = 1
    , displayAsString = show
    }

peanoNatural :: Natural Peano
peanoNatural = Natural
    { equal = eqPeano 
    , add = addPeano
    , multiply = multiplyPeano
    , additiveIdentity = Z
    , multiplicativeIdentity = S Z
    , displayAsString = show . fromPeano
    }

-- Examples: 
-- unique intNatural [1,2,3,4,2,5,1] => [1,2,3,4,5]
-- map fromPeano $ unique peanoNatural (map toPeano [1,2,3,4,2,5,1]) = [1,2,3,4,5]
unique :: Natural a -> [a] -> [a]
unique _ [] = []
unique n (elem:elems) =
    let
        compare a b = not $ (equal n) a b
        elems' = filter (compare elem) elems
    in elem : unique n elems'

sumOfUniques :: Natural a -> [a] -> a
sumOfUniques n = foldr (add n) (additiveIdentity n) . unique n
    




