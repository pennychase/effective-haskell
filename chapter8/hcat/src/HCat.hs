module HCat where

import qualified System.Environment as Env

handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
        parseArgs argumentList =
            case argumentList of
                [fname] -> Right fname
                []      -> Left "No filename provided"
                _       -> Left "Multiple files not supported"

 

runHCat :: IO ()
runHCat = 
    handleArgs >>= displayMessage
    where
        displayMessage parsedArgument =
            case parsedArgument of
                Left errMessage -> putStrLn $ "Error: " <> errMessage
                Right filename -> putStrLn $ "Opening file: " <> filename