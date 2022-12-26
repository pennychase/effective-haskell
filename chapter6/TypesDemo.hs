{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import NaturalClass 

showLeftRight :: (Read a, Read b) => String -> Either a b
showLeftRight s
    | length s > 5 = Left (read s)
    | otherwise = Right (read s)
    
-- TypeApplications lets us refer to the polymorphic type variables in the fucntion
-- defintion and we can use it to specifc the type for read
adheresToReadShowContract :: forall a. (Read a, Show a) => a -> Bool
adheresToReadShowContract val =
    let a = show . read @a . show $ val
        b = show val
    in a == b

-- make explicit type variables that are inferred
convertViaInt :: forall {a} b. (Integral a, Num b) => a -> b
convertViaInt input = fromIntegral $ fromIntegral @_ @Int input

-- This was the motivating example for the section on Type Applications
-- In addition to ScopedTypeVariables and TypeApplications, had to use AllowAmbiguousTypes
-- Example calls:
-- >>> showIdentities @Peano
-- >>> showIdentities @Int
showIdentities :: forall a. (Natural a) => IO ()
showIdentities =
    let mul = multiplicativeIdentity @a
        add = additiveIdentity @a
        msg = "The additivie identity is "
              <> show add
              <> " amd the multiplicativeIdentity is "
              <> show mul
    in print msg