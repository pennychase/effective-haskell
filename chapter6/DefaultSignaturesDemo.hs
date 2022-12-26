{-# LANGUAGE DefaultSignatures #-}

module DefaultSignaturesDemo where

class Redacted a where
    redacted :: a -> String
    default redacted :: Show a => a -> String
    redacted = show

data UserName = UserName String

instance Show UserName where
    show (UserName userName) = userName 

instance Redacted UserName

data Password = Password String

instance Redacted Password where
    redacted _ = "<redacted>"