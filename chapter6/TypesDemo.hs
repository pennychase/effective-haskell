{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}


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