{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DerivingVia #-}

module Selector where

import Data.Kind

-- typeclass for higher kinded types (simplified Alternative)

class Select (f :: Type -> Type) where
    empty :: f a
    pick :: f a -> f a -> f a

-- instances for Select

instance Select Maybe where
    empty = Nothing

    pick Nothing a = a
    pick a _ = a

instance Select [] where
    empty = []
    pick = (<>)


-- Deriving Via  example

-- newtype to wrap Select
-- Note that first argument matches the type of Select

newtype Sel (f :: Type -> Type) (a :: Type) = Sel (f a)
    deriving Show

-- instances for Sel using Select
-- By defining these we automatically have monoid instances for any type with kind of Type -> Type
-- and a Select instance

instance (Select f) => Semigroup (Sel f a) where
    (Sel a) <> (Sel b) = Sel (pick a b)

instance (Select f) => Monoid (Sel f a) where
    mempty = Sel empty

-- Using DervingVia to derive typeclass instance using the instance defined for a 
-- representationally equal type

newtype MyMaybe a = MyMaybe (Maybe a)
    deriving Show
    deriving (Semigroup, Monoid) via (Sel Maybe a)
