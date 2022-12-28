{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingVia where

import Prelude hiding (null)
import qualified Prelude (null)
import Data.Kind

-- Chapter 6 second exercise - provide API for using derving via to derive different types of Nullable for Maybe

newtype MyMaybe a = MyMaybe { getMyMaybe :: Maybe a }
    deriving Show

class Nullable a where
    isNull :: a -> Bool
    null :: a

instance Nullable [a] where
    isNull = Prelude.null
    null = []

instance Nullable (Maybe a) where
    isNull Nothing = True
    isNull _ = False
    null = Nothing

-- Nullable instance for MyMaybe
-- isNull is True when the value is Nothing or the null of a Nullable instance (e.g., MyMaybe (Just []))
instance Nullable a => Nullable (MyMaybe a) where
    null = MyMaybe Nothing
    isNull m  =
        case getMyMaybe m of
            Nothing -> True
            Just a -> isNull a

-- Nullable instance of a Maybe [a] derived via MyMaybe
newtype MaybeList1 a = MaybeList1 { getMaybeList1 :: Maybe [a] }
    deriving stock Show
    deriving Nullable via (MyMaybe [a])

-- Nullable instance of a Maybe [a] derived via Maybe
newtype MaybeList2 a = MaybeList2{ getMaybeList2 :: Maybe [a] }
    deriving stock Show
    deriving Nullable via (Maybe [a])







