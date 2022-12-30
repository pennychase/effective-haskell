module Exercises7 where

func :: IO (IO String)
func = putStrLn "I'm returning an IO (IO String)" >> return (return "String")

func2 :: IO (IO a) -> IO a
func2 f = 
    putStrLn "I'm returning an IO a" >>
        f >>= (\res -> res >>= (\res' -> return res'))