{-# LANGUAGE RecordWildCards #-}

{- |
This module is an example of how you can create a module.
This comment is an example of how to create a comment that will be 
in the haddock-generated documentation.
-}
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

{- |
A GreetingMessage contains the infomration needed to generate a greeting using
'formatMessage'. You can get a default greeting without attribution from
'defaultMessage'. This makes it convenient to use record update syntax to
construct a new greeting.

>>> formatMessage defaultMessage { greetingFrom = [ Name "A Haskeller" ] }
"Hello Friend, from: A Hakeller"
-}
data GreetingMessage = GreetingMessage
    { greetingSalutation :: Salutation -- ^ A 'Salutation' like \"Hello\"
    , greetingTo :: Name   -- ^ 'Name' of the person that should be greeted
    , greetingFrom :: [Name] -- ^ 'Name's of the people who are sending the greeting
    }

{- |
A default greeting message that isn't attributed to anyone. 

@
GreetingMessage
   { greetingSalutation = 'Salutation' \"Hello\"
    , greetingTo = 'Name' \"Friend\"
    , greetingFrom = []
    }
@
-}
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

{- |
A test message that you can use to see how messages are formatted.

>>> testMessgae
"Hello Friend, from: test example"
-}
testMessage :: String
testMessage = formatMessage $ defaultMessage { greetingFrom = [Name "test example"]}


