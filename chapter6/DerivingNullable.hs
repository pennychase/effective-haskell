{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DerivingStrategies #-}

module DerivingVia where

import Prelude hiding (null)
import qualified Prelude (null)

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

-- examples of using deriving via to derive Nullable instances for Maybe or MyMaybe
newtype MaybeListInt = MaybeListInt { getMaybeListInt :: Maybe [Int]}
    deriving stock Show
    deriving Nullable via (MyMaybe [Int])

newtype MaybeListString = MaybeListString { getMaybeListString :: Maybe String }
    deriving stock Show
    deriving Nullable via (Maybe String)