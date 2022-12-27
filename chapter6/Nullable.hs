{-# LANGUAGE KindSignatures #-}

module Nullable where

import Prelude hiding (null)

class Nullable a where
    isNull :: a -> Bool
    null :: a

instance Nullable (Maybe a) where
    isNull Nothing = True
    isNull _ = False

    null = Nothing

instance Nullable [a] where
    isNull [] = True
    isNull _ = False

    null = []