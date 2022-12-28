{-# LANGUAGE KindSignatures #-}

module Nullable where

import Prelude hiding (null)

-- Chapter 6 first exercise - add Eq class constraint and use it to define default isNull

-- Add an (Eq a) class constraint and define a default isNull
class Eq a => Nullable a where
    isNull :: a -> Bool
    isNull a = a == null

    null :: a

instance (Eq a) => Nullable (Maybe a) where
    null = Nothing

instance (Eq a) => Nullable [a] where
    null = []