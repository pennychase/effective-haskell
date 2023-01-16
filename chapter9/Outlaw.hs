module Outlaw where

import Data.Char

-- Demonstration of instance not obeying the Functor, Monad, and Applicative Laws

-- Outlaw type

data Outlaw a = Outlaw Int a
    deriving (Show, Eq)

-- an outlaw
billyTheKid = Outlaw 0 "bank robber"


-- Test Functor Laws

instance Functor Outlaw where
    fmap f (Outlaw cnt value) = Outlaw (cnt + 1) (f value)

-- helper functions
bang = (<> "!")

upcase = map Data.Char.toUpper 

-- tests
testIdentity = fmap id billyTheKid == id billyTheKid

testComposition = 
    fmap (bang . upcase) billyTheKid == (fmap bang . fmap upcase $ billyTheKid)


-- Test Monad laws 

-- need Applicative instance to make ghc happy
instance Applicative Outlaw where
    pure summary = Outlaw 0 summary
    (<*>) = undefined

instance Monad Outlaw where
    (Outlaw cnt a) >>= f =
        let (Outlaw cnt' v) = f a
        in Outlaw (cnt + cnt' + 1) v

-- helper functions
stoleAHorse :: String -> Outlaw String
stoleAHorse = return . (<> " and a horse robber")

wasAGunfigter :: String -> Outlaw String
wasAGunfigter = return . (<> ", was a gunfighter, ")

-- tests

testLeftIdentity =
    (return "robbed a bank" >>= stoleAHorse) == stoleAHorse "robbed a bank"

testRightIdentity =
    (billyTheKid >>= return) == billyTheKid

testAssociativity =
    ( (return "robbed a bank" >>= wasAGunfigter) >>= stoleAHorse ) ==
        (return "robbed a bank" >>= (\s -> wasAGunfigter s >>= stoleAHorse) )




