{-# LANGUAGE RecordWildCards #-}

module HaskellBook.Examples.UserInfo
    ( User
    , lookupUser
    , authenticate
    , getUserName
    , getUserScore
    , getUserEmailAddress
    ) where

import Data.List(find)

-- User Phantom Types to track if user is authenticated (don't need value constructors since we wantb to track at type level)

data Authenticated
data Unauthenticated

data User isAuthenticated = User
    { userName :: String
    , userInternetPoints :: Int
    , userPassword :: String
    , userEmailAddress :: String
    }

users :: [User a]
users = [george, porter]
    where
        george = User
            { userName = "george"
            , userInternetPoints = 1000
            , userPassword = "secret"
            , userEmailAddress = "gbird2015@example.com"
            }
        porter = User
            { userName = "porter"
            , userInternetPoints = 500
            , userPassword = "hunter2"
            , userEmailAddress = "woofwoof@example.com"
            }

authenticate :: User Unauthenticated -> String -> Maybe (User Authenticated)
authenticate User{..} password 
    | userPassword == password = Just User{..}
    | otherwise = Nothing

-- User is unathenticated until they login
lookupUser :: String -> Maybe (User Unauthenticated) 
lookupUser name = find (\user -> userName user == name) users

-- Can get name or score whether ot not logged in
getUserName :: User isAuthenticated -> String
getUserName = userName

getUserScore :: User isAuthenticated -> Int
getUserScore = userInternetPoints

-- Need to be logged in to get email address
getUserEmailAddress :: User Authenticated -> String
getUserEmailAddress = userEmailAddress

