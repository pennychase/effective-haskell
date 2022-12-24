{-# LANGUAGE RecordWildCards #-}

module HaskellBook.Examples.Introduction.CreatingModules 
    ( Name (..)
    , Salutation (..)
    , GreetingMessage (..)
    , defaultMessage
    , formatMessage
    , testMessage
    ) where


data Name = Name { getName :: String }

data Salutation = Salutation { getSalutation :: String }

data GreetingMessage = GreetingMessage
    { greetingSalutation :: Salutation
    , greetingTo :: Name
    , greetingFrom :: [Name]
    }

defaultMessage :: GreetingMessage
defaultMessage = GreetingMessage
    { greetingSalutation = Salutation "Hello"
    , greetingTo = Name "Friend"
    , greetingFrom = []
    }

formatMessage :: GreetingMessage -> String
formatMessage GreetingMessage{..} =
    greetingWithSuffix
    where
        greetingWithSuffix =
            case greetingFrom of
                [] -> basicGreeting <> "!"
                [friend] -> basicGreeting <> ", from: " <> getName friend
                [friendA,friendB] -> basicGreeting <> ", from: " <> getName friendA <> " and " <> getName friendB
                friends -> basicGreeting <> ", from your friends: " <> formatFriendList friends

        basicGreeting = getSalutation greetingSalutation <> " " <> getName greetingTo

        formatFriendList friends =
            case friends of
                [] -> ""
                [friend] -> "and " <> getName friend
                (friend:moreFriends) -> getName friend <> ", " <> formatFriendList moreFriends

testMessage :: String
testMessage = formatMessage $ defaultMessage { greetingFrom = [Name "test example"]}


