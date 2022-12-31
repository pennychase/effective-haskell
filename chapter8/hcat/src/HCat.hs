{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module HCat where

import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError
import qualified System.Environment as Env


-- Command line argument processing
handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
        parseArgs argumentList =
            case argumentList of
                [fname] -> Right fname
                []      -> Left "No filename provided"
                _       -> Left "Multiple files not supported"

-- Handle errors uniformly by turning an Either into an IO Error
eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) = Exception.throwIO . IOError.userError $ show e

runHCat :: IO ()
runHCat = 
    handleIOError $
        handleArgs
        >>= eitherToErr
        >>= readFile
        >>= putStrLn
    where
        handleIOError :: IO () -> IO ()
        handleIOError ioAction =
            Exception.catch ioAction $ 
                \e -> print "I ran into an error" >> print @IOError e -- use @IOError instead of writing a handler

