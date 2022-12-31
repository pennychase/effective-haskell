{-# LANGUAGE LambdaCase #-}

module HCat where

import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
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
runHCat = Exception.catch
    ( handleArgs 
        >>= \case 
        Left err -> putStrLn $ "Error processing: " <> err
        Right fname -> readFile fname >>= putStrLn
    ) handleErr
    where
        handleErr :: IOError -> IO ()
        handleErr e = putStrLn "I ran into an error! " >> print e

