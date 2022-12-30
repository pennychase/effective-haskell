module Replace where

import System.Environment (getArgs)



parseArgs :: [String] -> (String, String, String)
parseArgs strs = 
    if length strs /= 3
        then error "Wrong number of arguments"
        else (strs !! 0, strs !! 1, strs !! 2)

replace :: String -> String -> String -> String
replace input old new =
    unwords . map replace' $ words input
    where
        replace' str 
            | str == old = new
            | otherwise = str


main :: IO ()
main = 
    getArgs >>= \args ->
    let (path, needle, replacement) = parseArgs args
    in readFile path >>= \input -> print $ replace input needle replacement


