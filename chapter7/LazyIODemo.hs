module LazyIODemo where

import Text.Printf

ignoreUnevaluatedIO :: IO ()
ignoreUnevaluatedIO =
    let screamIntoTheVoid = putStrLn "quack"
    in return ()

-- Exception isn't raised because raiseMathError's return value is not evaluated
lazyIODemo :: IO ()
lazyIODemo =
    let 
        sayHello :: IO ()
        sayHello = putStrLn "Hello"

        raiseMathError :: IO Int
        raiseMathError = putStrLn "I'm part of math error"
                         >>  return (1 `div` 0)
     in sayHello >> raiseMathError >> sayHello

-- Accessing raiseMathError's return value raises the exception
forceError :: IO ()
forceError = do
    sayHello
    res <- raiseMathError
    putStrLn $ show res
    sayHello
    where
        sayHello :: IO ()
        sayHello = putStrLn "Hello"

        raiseMathError :: IO Int
        raiseMathError = putStrLn "I'm part of math error"
                         >>  return (1 `div` 0)

-- Unsafe version of writing and reading files
-- Exhausts number of file handles
makeAndReadFile :: Int -> IO String
makeAndReadFile fnumber =
    let fname = printf "/tmp/test/%d" fnumber
    in writeFile fname fname >> readFile fname

unsafe :: IO ()
unsafe =
    let files = mapM makeAndReadFile [1..500] :: IO [String]
    in files >>= (putStrLn . show)

-- Safe version of writing and reading files
makeAndShow :: Int -> IO ()
makeAndShow n = makeAndReadFile n >>= putStrLn 

safe :: IO ()
safe = mapM_ makeAndShow [1..500]
